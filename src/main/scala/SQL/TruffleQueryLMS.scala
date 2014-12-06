package SQL

import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import java.io.FileReader
import java.io.BufferedReader
import LMS.TruffleLMS

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
//      case Group(keys, agg, parent) =>
//        val hm = new HashMapAgg(keys, agg)
//        execOp(parent) { rec =>
//          hm(rec(keys)) += rec(agg)
//        }
//        hm foreach { (k, a) =>
//          yld(Record(k ++ a, keys ++ agg))
//        }
//      case HashJoin(left, right) =>
//        val keys = resultSchema(left) intersect resultSchema(right)
//        val hm = new HashMapBuffer(keys, resultSchema(left))
//        execOp(left) { rec1 =>
//          hm(rec1(keys)) += rec1.fields
//        }
//        execOp(right) { rec2 =>
//          hm(rec2(keys)) foreach { rec1 =>
//            yld(Record(rec1.fields ++ rec2.fields, rec1.schema ++ rec2.schema))
//          }
//        }
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
    
    
//    object hashDefaults {
//      val hashSize = (1 << 8)
//      val keysSize = hashSize
//      val bucketSize = (1 << 8)
//      val dataSize = keysSize * bucketSize
//    }
//
//    // common base class to factor out commonalities of group and join hash tables
//
//    class HashMapBase(keySchema: Schema, schema: Schema) {
//      import hashDefaults._
//
//      val keys = new ArrayBuffer[String](keysSize, keySchema)
//      val keyCount = var_new(0)
//
//      val hashMask = hashSize - 1
//      val htable = NewArray[Int](hashSize)
//      for (i <- 0 until hashSize) { htable(i) = -1 }
//
//      def lookup(k: Fields) = lookupInternal(k, None)
//      def lookupOrUpdate(k: Fields)(init: Rep[Int] => Rep[Unit]) = lookupInternal(k, Some(init))
//      def lookupInternal(k: Fields, init: Option[Rep[Int] => Rep[Unit]]): Rep[Int] = {
//        val h = fieldsHash(k).toInt
//        var pos = h & hashMask
//        while (htable(pos) != -1 && !fieldsEqual(keys(htable(pos)), k)) {
//          pos = (pos + 1) & hashMask
//        }
//        if (init.isDefined) {
//          if (htable(pos) == -1) {
//            val keyPos = keyCount: Rep[Int] // force read
//            keys(keyPos) = k
//            keyCount += 1
//            htable(pos) = keyPos
//            init.get(keyPos)
//            keyPos
//          } else {
//            htable(pos)
//          }
//        } else {
//          htable(pos)
//        }
//      }
//    }
//
//    // hash table for groupBy, storing sums
//
//    class HashMapAgg(keySchema: Schema, schema: Schema) extends HashMapBase(keySchema: Schema, schema: Schema) {
//      import hashDefaults._
//
//      val values = new ArrayBuffer[Int](keysSize, schema) // assuming all summation fields are numeric
//
//      def apply(k: Fields) = new {
//        def +=(v: Fields) = {
//          val keyPos = lookupOrUpdate(k) { keyPos =>
//            values(keyPos) = schema.map(_ => 0: Rep[Int])
//          }
//          values(keyPos) = (values(keyPos), v.map(_.toInt)).zipped map (_ + _)
//        }
//      }
//
//      def foreach(f: (Fields, Fields) => Rep[Unit]): Rep[Unit] = {
//        for (i <- 0 until keyCount) {
//          f(keys(i), values(i).map(_.ToString))
//        }
//      }
//
//    }
//
//    // hash table for joins, storing lists of records
//
//    class HashMapBuffer(keySchema: Schema, schema: Schema) extends HashMapBase(keySchema: Schema, schema: Schema) {
//      import hashDefaults._
//
//      val data = new ArrayBuffer[String](dataSize, schema)
//      val dataCount = var_new(0)
//
//      val buckets = NewArray[Int](dataSize)
//      val bucketCounts = NewArray[Int](keysSize)
//
//      def apply(k: Fields) = new {
//        def +=(v: Fields) = {
//          val dataPos = dataCount: Rep[Int] // force read
//          data(dataPos) = v
//          dataCount += 1
//
//          val bucket = lookupOrUpdate(k)(bucket => bucketCounts(bucket) = 0)
//          val bucketPos = bucketCounts(bucket)
//          buckets(bucket * bucketSize + bucketPos) = dataPos
//          bucketCounts(bucket) = bucketPos + 1
//        }
//
//        def foreach(f: Record => Rep[Unit]): Rep[Unit] = {
//          val bucket = lookup(k)
//
//          if (bucket != -1) {
//            val bucketLen = bucketCounts(bucket)
//            val bucketStart = bucket * bucketSize
//
//            for (i <- bucketStart until (bucketStart + bucketLen)) {
//              f(Record(data(buckets(i)), schema))
//            }
//          }
//        }
//      }
//    }
//
//    class ArrayBuffer[T:Typ:Manifest](dataSize: Int, schema: Schema) {
//      val buf = schema.map(f => NewArray[T](dataSize))
//      var len = 0
//      def +=(x: Seq[Rep[T]]) = {
//        this(len) = x
//        len += 1
//      }
//      def update(i: Rep[Int], x: Seq[Rep[T]]) = {
//        (buf, x).zipped.foreach((b, x) => b(i) = x)
//      }
//      def apply(i: Rep[Int]) = {
//        buf.map(b => b(i))
//      }
//    }

  }

}