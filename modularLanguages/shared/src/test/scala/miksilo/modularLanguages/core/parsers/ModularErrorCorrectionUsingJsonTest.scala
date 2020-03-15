package miksilo.modularLanguages.core.parsers

import miksilo.modularLanguages.deltas.json.ModularJsonLanguage
import org.scalatest.funsuite.AnyFunSuite
import util.TestLanguageBuilder

class ModularErrorCorrectionUsingJsonTest extends AnyFunSuite {

  test("test whether correct inputs always return a ready in one go") {
    val input = """{ "VpcId" : {
                  |  "ConstraintDescription" : "must be the VPC Id of an existing Virtual Private Cloud."
                  |}}""".stripMargin
    val result = TestLanguageBuilder.buildWithParser(ModularJsonLanguage.deltas).compileString(input)
    //parseJson(input, 3, 0)
  }
}
