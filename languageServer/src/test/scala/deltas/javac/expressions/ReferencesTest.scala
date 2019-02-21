package deltas.javac.expressions

import core.language.node.SourceRange
import deltas.javac.JavaLanguage
import languageServer.{HumanPosition, LanguageServerTest, MiksiloLanguageServer}
import org.scalatest.FunSuite
import util.SourceUtils

class ReferencesTest extends FunSuite with LanguageServerTest {

  val server = new MiksiloLanguageServer(JavaLanguage.java)

  val referenceRanges = Seq(SourceRange(HumanPosition(10,16), HumanPosition(10,21)),
    SourceRange(HumanPosition(10,52), HumanPosition(10,57)),
    SourceRange(HumanPosition(10,85), HumanPosition(10,90)))

  val definitionRange = SourceRange(HumanPosition(8,37), HumanPosition(8,42))

  test("onDeclarationWithoutDefinition") {
    val program = SourceUtils.getJavaTestFileContents("Fibonacci")
    val result = references(server, program, new HumanPosition(8, 38), includeDeclaration = false)
    assertResult(referenceRanges)(result.map(l => l.range))
  }

  test("onDeclarationWithDefinition") {
    val program = SourceUtils.getJavaTestFileContents("Fibonacci")
    val result = references(server, program, new HumanPosition(8, 38), includeDeclaration = true)
    assertResult(Seq(definitionRange) ++ referenceRanges)(result.map(l => l.range))
  }

  test("onReferenceWithoutDefinition") {
    val program = SourceUtils.getJavaTestFileContents("Fibonacci")
    val result = references(server, program, referenceRanges.head.start, includeDeclaration = false)
    assertResult(referenceRanges)(result.map(l => l.range))
  }

  test("onReferenceWithDefinition") {
    val program = SourceUtils.getJavaTestFileContents("Fibonacci")
    val result = references(server, program, referenceRanges.head.start, includeDeclaration = true)
    assertResult(Seq(definitionRange) ++ referenceRanges)(result.map(l => l.range))
  }
}
