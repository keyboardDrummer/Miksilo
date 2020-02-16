package languageServer

import core.language.Language
import core.parsers.editorParsers.{Position, SourceRange}
import lsp.{Diagnostic, DiagnosticSeverity, HumanPosition}
import org.scalatest.funsuite.AnyFunSuite

class ExampleExpressionLanguageTest extends AnyFunSuite with LanguageServerTest {

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

  test("diagnostics placement in whitespace 1") {
    val program = "    "

    val diagnostic = Diagnostic(SourceRange(Position(0,0), Position(0,4)), Some(DiagnosticSeverity.Error), "expected '<expression>'")
    val result = getDiagnostics(server, program)
    assertResult(Seq(diagnostic))(result)
  }

  // TODO seems to fail because of SREmpty with TODO in LeftRecursiveCorrectingParserWriter
  ignore("diagnostics placement in whitespace 2") {
    val program = "   + 3"

    val diagnostic = Diagnostic(SourceRange(Position(0,0), Position(0,3)), Some(DiagnosticSeverity.Error), "expected '<expression>'")
    val result = getDiagnostics(server, program)
    assertResult(Seq(diagnostic))(result)
  }

  test("diagnostics placement in whitespace 3") {
    val program = "3 +    "

    val diagnostic = Diagnostic(SourceRange(Position(0,3), Position(0,7)), Some(DiagnosticSeverity.Error), "expected '<expression>'")
    val result = getDiagnostics(server, program)
    assertResult(Seq(diagnostic))(result)
  }

  test("diagnostics placement in whitespace 5") {
    val program = "let   = 3 in abc"

    val diagnostic = Diagnostic(SourceRange(Position(0,3), Position(0,4)), Some(DiagnosticSeverity.Error), "expected '<identifier>'")
    val result = getDiagnostics(server, program)
    assertResult(Seq(diagnostic))(result)
  }

  test("diagnostics placement in whitespace 6") {
    val program = "let abc =      in abc"

    val diagnostic = Diagnostic(SourceRange(Position(0,9), Position(0,15)), Some(DiagnosticSeverity.Error), "expected '<expression>'")
    val result = getDiagnostics(server, program)
    assertResult(Seq(diagnostic))(result)
  }
}
