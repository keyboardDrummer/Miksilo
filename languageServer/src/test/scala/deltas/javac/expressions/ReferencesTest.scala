package deltas.javac.expressions

import deltas.javac.JavaLanguage
import langserver.types.Range
import languageServer.{HumanPosition, LanguageServerTest, MiksiloLanguageServer}
import org.scalatest.FunSuite
import util.SourceUtils

class ReferencesTest extends FunSuite with LanguageServerTest {

  val server = new MiksiloLanguageServer(JavaLanguage.getJava)

  test("fibonacci") {
    val program = SourceUtils.getJavaTestFileContents("Fibonacci")
    val result = references(server, program, new HumanPosition(8, 38))
    val resultRanges = Seq(Range(HumanPosition(10,16), HumanPosition(10,21)),
      Range(HumanPosition(10,52), HumanPosition(10,57)),
      Range(HumanPosition(10,85), HumanPosition(10,90)))
    assertResult(resultRanges)(result.map(l => l.range))
  }
}
