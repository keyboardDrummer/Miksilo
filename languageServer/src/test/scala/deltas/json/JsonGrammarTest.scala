package deltas.json

import core.bigrammar.TestLanguageGrammarUtils
import core.deltas.path.{NodePath, PathRoot}
import core.language.node.Node
import deltas.json.JsonObjectLiteralDelta.{MemberValue, ObjectLiteral}
import languageServer.Position
import org.scalatest.FunSuite
import util.TestLanguageBuilder

class JsonGrammarTest extends FunSuite {

  val utils = new TestLanguageGrammarUtils(JsonLanguage.deltas)

  test("testStringValueSourceLocation") {
    val example =
      """{
        |  "x": "stringValue"
        |}""".stripMargin

    val result: ObjectLiteral[NodePath] = PathRoot(utils.parse(example).asInstanceOf[Node])
    val member = result.members.head
    assertResult(Position(1, 7))(member.getField(MemberValue).range.get.start)
    assertResult(Position(1, 20))(member.getField(MemberValue).range.get.end)

    val stringValue = member.value
    val stringLocation = stringValue.getField(JsonStringLiteralDelta.Value)
    assertResult(Position(1, 8))(stringLocation.range.get.start)
    assertResult(Position(1, 19))(stringLocation.range.get.end)
  }

  test("missing object member key") {
    val program = """{:3}""".stripMargin
    val reference = """{"":3}""".stripMargin

    val jsonLanguage = TestLanguageBuilder.buildWithParser(JsonLanguage.deltas)
    val result = jsonLanguage.compile(program).program
    val expectedProgram = jsonLanguage.compile(reference).program
    assertResult(expectedProgram)(result)
  }

  test("parseSimpleJson") {
    val example =
      """{
        |  "x": 3
        |}""".stripMargin

    utils.compareInputWithPrint(example)
  }

  test("optionalComma") {
    val example =
      """{
        |  "x": 3,
        |}""".stripMargin

    val expected =
      """{
        |  "x": 3
        |}""".stripMargin

    val ast = utils.parse(example)
    val printResult = utils.getPrintResult(ast)
    assertResult(expected)(printResult)
  }
}
