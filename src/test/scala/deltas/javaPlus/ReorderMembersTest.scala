package deltas.javaPlus

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import application.compilerCockpit.PrettyPrint
import deltas.javac.JavaCompilerDeltas
import deltas.javac.trivia.{CaptureTriviaDelta, JavaStyleCommentsDelta, TriviaInsideNode}
import org.scalatest.FunSuite
import util.CompilerBuilder

class ReorderMembersTest extends FunSuite {


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

    val compiler = CompilerBuilder.build(Seq(ReorderMembers) ++ JavaCompilerDeltas.prettyPrintJavaDeltas)

    val inputStream = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))
    assertThrows[core.grammar.ParseException]({
      val state = compiler.parseAndTransform(inputStream)
      assertResult(null)(state.output)
    })
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
    val compiler = CompilerBuilder.build(Seq(ReorderMembers) ++ JavaCompilerDeltas.prettyPrintJavaDeltas)

    val inputStream = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))
    val state = compiler.parseAndTransform(inputStream)
    assertResult(expectation)(state.output)
  }

  test("broken comment") {
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
    val compiler = CompilerBuilder.build(Seq(ReorderMembers, PrettyPrint(),
      JavaStyleCommentsDelta, CaptureTriviaDelta) ++
      JavaCompilerDeltas.javaCompilerDeltas)

    val inputStream = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))
    val state = compiler.parseAndTransform(inputStream)
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
    val compiler = CompilerBuilder.build(Seq(ReorderMembers, PrettyPrint(),
      JavaStyleCommentsDelta, CaptureTriviaDelta, TriviaInsideNode) ++
      JavaCompilerDeltas.javaCompilerDeltas)

    val inputStream = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))
    val state = compiler.parseAndTransform(inputStream)
    assertResult(expectation)(state.output)
  }
}
