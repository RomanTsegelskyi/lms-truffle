import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import scala.annotation.target.field
import org.scalatest._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import trufflequery.Scanner

trait QueryBase {

  def resultSchema(o: OperatorNode): Schema = o match {
    case ScanNode(_, schema, _, _) => schema
    case FilterNode(pred, parent)    => resultSchema(parent)
    case ProjectNode(schema, _, _)   => schema
    case JoinNode(left, right)       => resultSchema(left) ++ resultSchema(right)
    case GroupNode(keys, agg, parent)=> keys ++ agg
    case HashJoinNode(left, right)   => resultSchema(left) ++ resultSchema(right)
    case PrintCSVNode(parent)        => Schema()
  }

  val defaultFieldDelimiter = ','

  def printSchema(schema: Schema) = println(schema.mkString(defaultFieldDelimiter.toString))

  def printFields(fields: Fields) = printf(fields.map { _ => "%s" }.mkString("", defaultFieldDelimiter.toString, "\n"), fields: _*)

  // base types
  abstract class BaseNode extends Node with Product {
    def prettyString = productPrefix + "(" + ((0 until productArity) map productElement mkString ",") + ")"
    override def toString = prettyString
  }

  type Table
  type Schema = Vector[String]
  type Fields = Vector[String]

  case class Record(fields: Fields, schema: Schema) {
    def apply(key: String): String = fields(schema indexOf key)
    def apply(keys: Schema): Fields = keys.map(this apply _)
  }

  // relational algebra ops
  abstract class OperatorNode extends BaseNode {
    def execute(frame: VirtualFrame)(yld: Record => Unit)
  }

  case class ScanNode(filename: String, schema: Schema, fieldDelimiter: Char, externalSchema: Boolean) extends OperatorNode {
    def execute(f: VirtualFrame)(yld: Record => Unit): Unit = {
      val s = new Scanner(filename)
      val last = schema.last
      def nextRecord = Record(schema.map { x => s.next(if (x == last) '\n' else fieldDelimiter) }, schema)
      if (!externalSchema) {
        nextRecord
      }
      while (s.hasNext) yld(nextRecord)
      s.close
    }
  }

  case class PrintCSVNode(parent: OperatorNode) extends OperatorNode {
    def execute(frame: VirtualFrame)(yld: Record => Unit) = {
      val schema = resultSchema(parent)
      printSchema(schema)
      parent.execute(frame) { rec => printFields(rec.fields) }
    }
  }

  case class ProjectNode(outSchema: Schema, inSchema: Schema, parent: OperatorNode) extends OperatorNode {
    def execute(frame: VirtualFrame)(yld: Record => Unit) = {
      parent.execute(frame) { rec => yld(Record(rec(inSchema), outSchema)) }
    }
  }

  case class FilterNode(pred: Predicate, parent: OperatorNode) extends OperatorNode {
    def execute(frame: VirtualFrame)(yld: Record => Unit) = {
      parent.execute(frame) { rec => if (evalPred(pred)(rec)) yld(rec) }
    }
  }

  case class JoinNode(left: OperatorNode, right: OperatorNode) extends OperatorNode {
    def execute(frame: VirtualFrame)(yld: Record => Unit) = {
      left.execute(frame) { rec1 =>
        right.execute(frame) { rec2 =>
          val keys = rec1.schema intersect rec2.schema
          if (rec1(keys) == rec2(keys))
            yld(Record(rec1.fields ++ rec2.fields, rec1.schema ++ rec2.schema))
        }
      }
    }
  }

  case class GroupNode(keys: Schema, agg: Schema, parent: OperatorNode) extends OperatorNode {
    def execute(frame: VirtualFrame)(yld: Record => Unit) = {
      val hm = new HashMap[Fields, Seq[Int]]
      parent.execute(frame) { rec =>
        val kvs = rec(keys)
        val sums = hm.getOrElseUpdate(kvs, agg.map(_ => 0))
        hm(kvs) = (sums, rec(agg).map(_.toInt)).zipped map (_ + _)
      }
      hm foreach {
        case (k, a) =>
          yld(Record(k ++ a.map(_.toString), keys ++ agg))
      }
    }
  }

  case class HashJoinNode(left: OperatorNode, right: OperatorNode) extends OperatorNode {
    def execute(frame: VirtualFrame)(yld: Record => Unit) = {
      val keys = resultSchema(left) intersect resultSchema(right)
      val hm = new HashMap[Fields, ArrayBuffer[Record]]
      left.execute(frame) { rec1 =>
        val buf = hm.getOrElseUpdate(rec1(keys), new ArrayBuffer[Record])
        buf += rec1
      }
      right.execute(frame) { rec2 =>
        hm.get(rec2(keys)) foreach {
          _.foreach { rec1 =>
            yld(Record(rec1.fields ++ rec2.fields, rec1.schema ++ rec2.schema))
          }
        }
      }
    }
  }

  // filter predicates
  abstract class Predicate
  case class Eq(a: Ref, b: Ref) extends Predicate

  sealed abstract class Ref
  case class Field(name: String) extends Ref
  case class Value(x: Any) extends Ref

  def evalPred(p: Predicate)(rec: Record): Boolean = p match {
    case Eq(a1, a2) => evalRef(a1)(rec) == evalRef(a2)(rec)
  }

  def evalRef(r: Ref)(rec: Record): String = r match {
    case Field(name) => rec(name)
    case Value(x) => x.toString
  }
  
  def Schema(schema: String*): Schema = schema.toVector
}
