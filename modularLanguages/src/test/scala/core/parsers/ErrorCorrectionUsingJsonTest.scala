package core.parsers

import deltas.json.JsonLanguage
import org.scalatest.FunSuite
import util.TestLanguageBuilder

class ErrorCorrectionUsingJsonTest extends FunSuite {

  test("test whether correct inputs always return a ready in one go") {
    val input = """{ "VpcId" : {
                  |  "ConstraintDescription" : "must be the VPC Id of an existing Virtual Private Cloud."
                  |}}""".stripMargin
    val result = TestLanguageBuilder.buildWithParser(JsonLanguage.deltas).compileString(input)
    //parseJson(input, 3, 0)
  }
}
