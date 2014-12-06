package SQL
import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import LMS.TruffleLMS

trait QueryProcessor extends QueryAST {
  type Table
  def version: String

  def filePath(table: String): String 
  def dynamicFilePath(table: String): Table

  def Scan(tableName: String, schema: Option[Schema], delim: Option[Char]): Scan = {
    val dfile = dynamicFilePath(tableName)
    val (schema1, externalSchema) = schema.map(s => (s, true)).getOrElse((loadSchema(filePath(tableName)), false))
    Scan(dfile, schema1, delim.getOrElse(defaultFieldDelimiter), externalSchema)
  }

  def loadSchema(filename: String): Schema = {
    val s = new Scanner(filename)
    val schema = Schema(s.next('\n').split(defaultFieldDelimiter): _*)
    s.close
    schema
  }

  def execQuery(o: Operator)
}

trait PlainQueryProcessor extends QueryProcessor {
  type Table = String
}

trait StagedQueryProcessor extends QueryProcessor with TruffleLMS {
  type Table = Rep[String] // dynamic filename
// def filePath(table: String) = if (table == "?") throw new Exception("file path for table ? not available") else super.filePath(table)
}
