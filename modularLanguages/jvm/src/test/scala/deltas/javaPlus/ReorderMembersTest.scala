package deltas.javaPlus

import deltas.javac.JavaToByteCodeLanguage
import deltas.trivia.{SlashStarBlockCommentsDelta, StoreTriviaDelta, TriviaInsideNode}
import deltas.{ClearPhases, PrettyPrint}
import org.scalatest.funsuite.AnyFunSuite
import util.TestLanguageBuilder

class ReorderMembersTest extends AnyFunSuite {

  test("cannot parse comment") {
    val input =
      """class Example
        |{
        |    int first;
        |
        |    /* second is for XYZ */
        |    public static /* global state XOXO */ int second;
        |
        |    /* third comes last */
        |    int third;
        |}""".stripMargin

    val compiler = TestLanguageBuilder.buildWithParser(Seq(ClearPhases, ReorderMembersDelta) ++ JavaToByteCodeLanguage.prettyPrintJavaDeltas)

    val compilation = compiler.compileString(input)
    assert(compilation.diagnostics.nonEmpty)
  }

  test("basic") {
    val input =
      """class Example
        |{
        |    int first;
        |
        |    static int second;
        |
        |    int third;
        |}""".stripMargin

    val expectation =
      """class Example
        |{
        |    static int second;
        |
        |    int first;
        |
        |    int third;
        |}""".stripMargin
    val compiler = TestLanguageBuilder.buildWithParser(Seq(ReorderMembersDelta.ActuallyReorderMembers) ++ JavaToByteCodeLanguage.prettyPrintJavaDeltas)

    val state = compiler.compileString(input)
    assertResult(expectation)(state.output)
  }

  test("comment without trivia inside node") {
    val input =
      """class Example
        |{
        |    int first;
        |
        |    /* second is for XYZ */
        |    public static /* global state XOXO */ int second;
        |
        |    /* third comes last */
        |    int third;
        |}""".stripMargin

    val expectation =
      """class Example
        |{
        |    public static /* global state XOXO */ int second;
        |    /* second is for XYZ */
        |
        |    int first;
        |    /* third comes last */
        |
        |    int third;
        |}""".stripMargin
    val compiler = TestLanguageBuilder.buildWithParser(Seq(ReorderMembersDelta.ActuallyReorderMembers, PrettyPrint(),
      SlashStarBlockCommentsDelta, StoreTriviaDelta) ++
      JavaToByteCodeLanguage.javaCompilerDeltas)

    val state = compiler.compileString(input)
    assertResult(expectation)(state.output)
  }

  test("comment") {
    val input =
      """class Example
        |{
        |    int first;
        |
        |    /* second is for XYZ */
        |    static /* global state XOXO */ int second;
        |
        |    /* third comes last */
        |    int third;
        |}""".stripMargin

    val expectation =
      """class Example
        |{
        |    /* second is for XYZ */
        |    static /* global state XOXO */ int second;
        |
        |    int first;
        |
        |    /* third comes last */
        |    int third;
        |}""".stripMargin
    val compiler = TestLanguageBuilder.buildWithParser(Seq(ReorderMembersDelta.ActuallyReorderMembers, PrettyPrint(),
      SlashStarBlockCommentsDelta, StoreTriviaDelta, TriviaInsideNode) ++
      JavaToByteCodeLanguage.javaCompilerDeltas)

    val state = compiler.compileString(input)
    assertResult(expectation)(state.output)
  }
}
