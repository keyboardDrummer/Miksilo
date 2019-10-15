package languageServer

import core.language.Language
import org.scalatest.FunSuite

class ExampleExpressionLanguageTest extends FunSuite with LanguageServerTest {

  val language: Language = ExampleExpressionLanguage
  val program = "let x = 3 in x + 3 + x"
  val server = new MiksiloLanguageServer(language)

  test("parse number") {
    assert(getDiagnostics(server, "3").isEmpty)
  }

  test("parse addition") {
    assert(getDiagnostics(server, "3 + 3").isEmpty)
  }

  test("parse let") {
    assert(getDiagnostics(server, "let x = 3 in x").isEmpty)
  }

  test("go to definition") {
    val firstXUsage = HumanPosition(1, 14)
    val secondXUsage = HumanPosition(1, 22)
    val xDefinition = SourceRange(HumanPosition(1, 5), HumanPosition(1, 6))

    assertResult(xDefinition)(gotoDefinition(server, program, firstXUsage).head.range)
    assertResult(xDefinition)(gotoDefinition(server, program, secondXUsage).head.range)
  }

  test("go to references") {
    val firstXUsage = SourceRange(HumanPosition(1, 14),HumanPosition(1, 15))
    val secondXUsage = SourceRange(HumanPosition(1, 22),HumanPosition(1, 23))
    val xPosition = HumanPosition(1, 5)
    val results = references(server, program, xPosition, includeDeclaration = false)

    assertResult(Seq(firstXUsage,secondXUsage))(results.map(r => r.range))
  }

  test("completion for correct program") {
    val program = "let abc = 3 in a"
    val beforeA = HumanPosition(1, 16)
    val afterA = HumanPosition(1, 17)

    val abcCompletion = createCompletionItem("abc")
    assertResult(Seq(abcCompletion))(complete(server, program, beforeA).items)
    assertResult(Seq(abcCompletion))(complete(server, program, afterA).items)
  }

  test("completion on identifier hole") {
    val program = "let abc = 3 in  "
    val beforeA = HumanPosition(1, 16)
    val afterA = HumanPosition(1, 17)

    val abcCompletion = createCompletionItem("abc")
    // TODO be able to turn this on
    //assertResult(Seq(abcCompletion))(complete(server, program, beforeA).items)
    assertResult(Seq(abcCompletion))(complete(server, program, afterA).items)
  }
}
