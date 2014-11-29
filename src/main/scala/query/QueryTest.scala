package query


class QueryTest extends TutorialFunSuite {
  val under = "query_"

  trait TestDriver extends SQLParser with QueryProcessor with ExpectedASTs {
    def runtest: Unit
    override def filePath(table: String) = dataFilePath(table)

    def name: String
    def query: String
    def parsedQuery: Operator = {
      if (query.isEmpty) expectedAstForTest(name) else parseSql(query)
    }
  }

  trait PlainTestDriver extends TestDriver with PlainQueryProcessor {
    override def dynamicFilePath(table: String): Table = if (table == "?") defaultEvalTable else filePath(table)
    def eval(fn: Table): Unit = {
    		execQuery(PrintCSV(parsedQuery))
    }
  }

  abstract class ScalaPlainQueryDriver(val name: String, val query: String) extends PlainTestDriver with QueryProcessor { q =>
    override def runtest: Unit = {
      test(version+" "+name) {
        for (expectedParsedQuery <- expectedAstForTest.get(name)) {
          assert(expectedParsedQuery==parsedQuery)
        }
        checkOut(name, "csv", eval(defaultEvalTable))
      }
    }
  }


  def testquery(name: String, query: String = "") {
    val drivers: List[TestDriver] =
      List(
        new ScalaPlainQueryDriver(name, query) with query_unstaged.QueryInterpreter
      )
    drivers.foreach(_.runtest)
  }

  // NOTE: we can use "select * from ?" to use dynamic file names (not used here right now)

  trait ExpectedASTs extends QueryAST {
    val scan_t = Scan("t.csv")
    val scan_t1gram = Scan("?",Some(Schema("Phrase", "Year", "MatchCount", "VolumeCount")),Some('\t'))

    val expectedAstForTest = Map(
      "t1" -> scan_t,
      "t2" -> Project(Schema("Name"), Schema("Name"), scan_t),
      "t3" -> Project(Schema("Name"), Schema("Name"),
                      Filter(Eq(Field("Flag"), Value("yes")),
                             scan_t)),
      "t4" -> Join(scan_t,
                   Project(Schema("Name1"), Schema("Name"), scan_t)),
      "t5" -> Join(scan_t,
                   Project(Schema("Name"), Schema("Name"), scan_t)),
      "t4h" -> HashJoin(scan_t,
                   Project(Schema("Name1"), Schema("Name"), scan_t)),
      "t5h" -> HashJoin(scan_t,
                   Project(Schema("Name"), Schema("Name"), scan_t)),
      "t6"  -> Group(Schema("Name"),Schema("Value"), scan_t),

      "t1gram1" -> scan_t1gram,
      "t1gram2" -> Filter(Eq(Field("Phrase"), Value("Auswanderung")), scan_t1gram)
    )
  }
  testquery("t1", "select * from t.csv")
//  testquery("t2", "select Name from t.csv")
 // testquery("t3", "select Name from t.csv where Flag='yes'")
//  testquery("t4", "select * from nestedloops t.csv join (select Name as Name1 from t.csv)")
//  testquery("t5", "select * from nestedloops t.csv join (select Name from t.csv)")
//  testquery("t4h","select * from t.csv join (select Name as Name1 from t.csv)")
//  testquery("t5h","select * from t.csv join (select Name from t.csv)")
//  testquery("t6", "select * from t.csv group by Name sum Value") // not 100% right syntax, but hey ...
//
  val defaultEvalTable = dataFilePath("t1gram.csv")
//  val t1gram = "? schema Phrase, Year, MatchCount, VolumeCount delim \\t"
//  testquery("t1gram1", s"select * from $t1gram")
//  testquery("t1gram2", s"select * from $t1gram where Phrase='Auswanderung'")
//  testquery("t1gram2n", s"select * from nestedloops words.csv join (select Phrase as Word, Year, MatchCount, VolumeCount from $t1gram)")
//  testquery("t1gram2h", s"select * from words.csv join (select Phrase as Word, Year, MatchCount, VolumeCount from $t1gram)")
//  testquery("t1gram3", s"select * from nestedloops words.csv join (select * from $t1gram)")
//  testquery("t1gram3h", s"select * from words.csv join (select * from $t1gram)")
//  testquery("t1gram4", s"select * from nestedloops words.csv join (select Phrase as Word, Year, MatchCount, VolumeCount from $t1gram)")
//  testquery("t1gram4h", s"select * from words.csv join (select Phrase as Word, Year, MatchCount, VolumeCount from $t1gram)")
}
