package sql

import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import java.io.FileReader
import java.io.BufferedReader
import lms.TruffleLMS

object query_staged0 {
  trait QueryCompiler extends TruffleLMS with StagedQueryProcessor {
    override def version = "query_staged0"

    /**
     * Low-Level Processing Logic
     * --------------------------
     */
    type Fields = Vector[Rep[String]]

    case class Record(fields: Fields, schema: Schema) {
      def apply(key: String): Rep[String] = fields(schema indexOf key)
      def apply(keys: Schema): Fields = keys.map(this apply _)
    }

    case class PrintFields(fields: Fields) extends Def[Unit] {
      def execute(frame: VirtualFrame) = {
        val f = fields.map { x => x.execute(frame) }
        printf(f.map { _ => "%s" }.mkString("", ','.toString, "\n"), f: _*)
      }
    }

    def processCSV(filename: Rep[String], schema: Schema, fieldDelimiter: Char, externalSchema: Boolean)(yld: Record => Rep[Unit]): Rep[Unit] = {
      val s = newScanner(filename)
      val last = schema.last
      def nextRecord = Record(schema.map { x => s.next(if (x == last) '\n' else fieldDelimiter) }, schema)
      if (!externalSchema) {
        nextRecord // ignore csv header
      }
      whileloop(s.hasNext) { yld(nextRecord) }
      s.close
    }

    def printSchema(schema: Schema) = println(schema.mkString(defaultFieldDelimiter.toString))

    def printFields(fields: Fields) = reflect(PrintFields(fields))

    def fieldsEqual(a: Fields, b: Fields) = (a zip b).foldLeft(lift(true)) { (a, b) => a && b._1 === b._2 }

    /**
     * Query Interpretation = Compilation
     * ----------------------------------
     */
    def evalPred(p: Predicate)(rec: Record): Rep[Boolean] = p match {
      case Eq(a1, a2) => evalRef(a1)(rec) === evalRef(a2)(rec)
    }

    def evalRef(r: Ref)(rec: Record): Rep[String] = r match {
      case Field(name) => rec(name)
      case Value(x) => x.toString
    }

    def resultSchema(o: Operator): Schema = o match {
      case Scan(_, schema, _, _) => schema
      case Filter(pred, parent) => resultSchema(parent)
      case Project(schema, _, _) => schema
      case Join(left, right) => resultSchema(left) ++ resultSchema(right)
      case Group(keys, agg, parent) => keys ++ agg
      case HashJoin(left, right) => resultSchema(left) ++ resultSchema(right)
      case PrintCSV(parent) => Schema()
    }

    def execOp(o: Operator)(yld: Record => Rep[Unit]): Unit = o match {
      case Scan(filename, schema, fieldDelimiter, externalSchema) =>
        processCSV(filename, schema, fieldDelimiter, externalSchema)(yld)
      case Filter(pred, parent) =>
        execOp(parent) { rec => cond(evalPred(pred)(rec)) { yld(rec) } {} }
      case Project(newSchema, parentSchema, parent) =>
        execOp(parent) { rec => yld(Record(rec(parentSchema), newSchema)) }
      case Join(left, right) =>
        execOp(left) { rec1 =>
          execOp(right) { rec2 =>
            val keys = rec1.schema intersect rec2.schema
            cond(fieldsEqual(rec1(keys), rec2(keys))) {
              yld(Record(rec1.fields ++ rec2.fields, rec1.schema ++ rec2.schema))
            } {}
          }
        }
      case PrintCSV(parent) =>
        val schema = resultSchema(parent)
        printSchema(schema)
        execOp(parent) { rec => printFields(rec.fields) }
    }

    def execQuery(q: Operator): Unit = {
      runtime = Truffle.getRuntime();
      frameDescriptor = new FrameDescriptor();
      val pr = lms { x: Rep[Unit] => execOp(q) { _ => } }
      pr();
    }
  }
}