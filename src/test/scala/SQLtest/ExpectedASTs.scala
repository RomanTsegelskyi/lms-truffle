package SQLtest
import SQL.QueryAST

trait ExpectedASTs extends QueryAST {
  val scan_t = Scan("t.csv")
  val scan_t1gram = Scan("?", Some(Schema("Phrase", "Year", "MatchCount", "VolumeCount")), Some('\t'))

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
    "t6" -> Group(Schema("Name"), Schema("Value"), scan_t),

    "t1gram1" -> scan_t1gram,
    "t1gram2" -> Filter(Eq(Field("Phrase"), Value("Auswanderung")), scan_t1gram))
}