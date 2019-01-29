package deltas.javac.expressions

import deltas.javac.JavaToByteCodeLanguage
import langserver.types.Range
import languageServer.{HumanPosition, LanguageServerTest, MiksiloLanguageServer}
import org.scalatest.FunSuite
import util.SourceUtils

class ReferencesTest extends FunSuite with LanguageServerTest {

  val server = new MiksiloLanguageServer(JavaToByteCodeLanguage.getJavaFrontend)

  val referenceRanges = Seq(Range(HumanPosition(10,16), HumanPosition(10,21)),
    Range(HumanPosition(10,52), HumanPosition(10,57)),
    Range(HumanPosition(10,85), HumanPosition(10,90)))

  val definitionRange = Range(HumanPosition(8,37), HumanPosition(8,42))

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
