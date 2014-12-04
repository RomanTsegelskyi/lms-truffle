package SQL

import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import org.scalatest._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.annotation.meta.field

object query_unstaged {
  trait QueryBase extends PlainQueryProcessor {

    /**
     * Low-Level Processing Logic
     * --------------------------
     */
    type Fields = Vector[String]
    def version = "unstaged"
    case class Record(fields: Fields, schema: Schema) {
      def apply(key: String): String = fields(schema indexOf key)
      def apply(keys: Schema): Fields = keys.map(this apply _)
    }

    abstract class OperatorNode extends Node with Product {
      def prettyString = productPrefix + "(" + ((0 until productArity) map productElement mkString ",") + ")"
      override def toString = prettyString
      def execute(frame:VirtualFrame):List[Record]
    }

    case class ProcessCSVNode(filename: String, schema: Schema, fieldDelimiter: Char, externalSchema: Boolean) extends OperatorNode {
      def execute(frame: VirtualFrame): List[Record] = {
        var res = List[Record]();
        val s = new Scanner(filename)
        val last = schema.last
        def nextRecord = Record(schema.map { x => s.next(if (x == last) '\n' else fieldDelimiter) }, schema)
        if (!externalSchema) {
          // the right thing would be to dynamically re-check the schema,
          // but it clutters the generated code
          // schema.foreach(f => if (s.next != f) println("ERROR: schema mismatch"))
          nextRecord // ignore csv header
        }
        while (s.hasNext) {
          val r = nextRecord
          res = res :+ r
        }
        s.close
        res
      }
    }

    case class ProjectNode(newSchema: Schema, parentSchema: Schema, parent: OperatorNode) extends OperatorNode {
      def execute(frame: VirtualFrame): List[Record] = {
        val records = parent.execute(frame);
        val l = records.map { x => Record(x(parentSchema), newSchema) }
        l
      }
    }

    case class FilterNode(pred: Predicate, parent: OperatorNode) extends OperatorNode {
      def execute(frame: VirtualFrame): List[Record] = {
        val records = parent.execute(frame);
        val l = records.filter { x => evalPred(pred)(x) }
        l
      }
    }

    case class JoinNode(left: OperatorNode, right: OperatorNode) extends OperatorNode {
      def execute(frame: VirtualFrame): List[Record] = {
        val leftr = left.execute(frame);
        val rightr = right.execute(frame);
        var res = List[Record]();
        leftr.foreach {
          rec1 =>
            rightr.foreach { rec2 =>
              val keys = rec1.schema intersect rec2.schema
              if (rec1(keys) == rec2(keys))
                res = res :+ Record(rec1.fields ++ rec2.fields, rec1.schema ++ rec2.schema)

            }
        }
        res
      }
    }

    case class GroupNode(keys: Schema, agg: Schema, parent: OperatorNode) extends OperatorNode {
      def execute(frame: VirtualFrame): List[Record] = {
        val hm = new HashMap[Fields, Seq[Int]]
        val records = parent.execute(frame);
        var res = List[Record]();
        records.foreach { rec =>
          val kvs = rec(keys)
          val sums = hm.getOrElseUpdate(kvs, agg.map(_ => 0))
          hm(kvs) = (sums, rec(agg).map(_.toInt)).zipped map (_ + _)
        }
        hm.foreach {
          case (k, a) =>
            res = res :+ Record(k ++ a.map(_.toString), keys ++ agg)
        }
        res
      }
    }

    case class HashJoinNode(left: OperatorNode, right: OperatorNode) extends OperatorNode {
      def execute(frame: VirtualFrame): List[Record] = {
        val keys = resultSchema(left) intersect resultSchema(right)
        val hm = new HashMap[Fields, ArrayBuffer[Record]]
        var res = List[Record]();
        val recl = left.execute(frame);
        val recr = right.execute(frame);
        recl.foreach { rec1 =>
          val buf = hm.getOrElseUpdate(rec1(keys), new ArrayBuffer[Record])
          buf += rec1
        }

        recr.foreach { rec2 =>
          hm.get(rec2(keys)) foreach {
            _.foreach { rec1 =>
              res = res :+ Record(rec1.fields ++ rec2.fields, rec1.schema ++ rec2.schema) 
            }
          }
        }
        res
      }
    }

    case class PrintCSVNode(parent: OperatorNode) extends OperatorNode {
      def execute(frame: VirtualFrame) = {
        val schema = resultSchema(parent)
        printSchema(schema)
        parent.execute(frame).foreach { rec => printFields(rec.fields) }
        List[Record]();
      }
    }

    def printSchema(schema: Schema) = println(schema.mkString(defaultFieldDelimiter.toString))

    def printFields(fields: Fields) = printf(fields.map { _ => "%s" }.mkString("", defaultFieldDelimiter.toString, "\n"), fields: _*)

    def evalPred(p: Predicate)(rec: Record): Boolean = p match {
      case Eq(a1, a2) => evalRef(a1)(rec) == evalRef(a2)(rec)
    }

    def evalRef(r: Ref)(rec: Record): String = r match {
      case Field(name) => rec(name)
      case Value(x) => x.toString
    }

    import scala.collection.mutable.{ ArrayBuffer, HashMap }

    def resultSchema(o: OperatorNode): Schema = o match {
      case ProcessCSVNode(_, schema, _, _) => schema
      case FilterNode(pred, parent) => resultSchema(parent)
      case ProjectNode(schema, _, _) => schema
      case JoinNode(left, right) => resultSchema(left) ++ resultSchema(right)
      case GroupNode(keys, agg, parent) => keys ++ agg
      case HashJoinNode(left, right) => resultSchema(left) ++ resultSchema(right)
      case PrintCSVNode(parent) => Schema()
    }

    def execOp(o: Operator): OperatorNode = o match {
      case Scan(filename, schema, fieldDelimiter, externalSchema) =>
        new ProcessCSVNode(filename, schema, fieldDelimiter, externalSchema)
      case Project(newSchema, parentSchema, parent) =>
        new ProjectNode(newSchema, parentSchema, execOp(parent))
      case Filter(pred, parent) =>
        new FilterNode(pred, execOp(parent))
      case Join(left, right) =>
        new JoinNode(execOp(left), execOp(right))
      case Group(keys, agg, parent) =>
        new GroupNode(keys, agg, execOp(parent))
      case HashJoin(left, right) =>
        new HashJoinNode(execOp(left), execOp(right))
      case PrintCSV(parent) =>
        new PrintCSVNode(execOp(parent))
    }
    def execQuery(q: Operator): RootNode = {
      class TRootNode(desc: FrameDescriptor, @(Child @field) val block: OperatorNode) extends RootNode(null, desc) {
        def execute(frame: VirtualFrame): List[Record] = block.execute(frame)
      }
      val descriptor: FrameDescriptor = new FrameDescriptor()
      new TRootNode(descriptor, execOp(q));
    }
  }
}
