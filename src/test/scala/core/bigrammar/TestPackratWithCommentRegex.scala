package core.bigrammar

import core.particles.node.GrammarKey
import org.scalatest.FunSuite
import transformations.javac.JavaStyleCommentsC

import scala.util.matching.Regex
import scala.util.parsing.combinator.{JavaTokenParsers, PackratParsers}
import scala.util.parsing.input.CharArrayReader

case class StringKey(value: String) extends GrammarKey
class TestPackratWithCommentRegex extends FunSuite with JavaTokenParsers with PackratParsers with BiGrammarSequenceWriter {

  test("PlusMinus") {
    lazy val commentParser: PackratParser[Any] = regex(new Regex( """/\*.*\*/"""))*
    lazy val parser: PackratParser[Any] =
        commentParser ~ parser ~ literal("+") ~ parser |||
        commentParser ~ parser ~ literal("-") ~ parser |||
        commentParser ~ wholeNumber
    val input = "/* jo */ 2 + 3"
    val reader = new PackratReader[Char](new CharArrayReader(input.toCharArray))
    val result = parser(reader)
  }

  test("MinusPlusRightCommutationBiGrammar4") {
    val commentParser: BiGrammar = RegexG(new Regex("""/\*.*\*/""")).manyVertical
    val core = new Labelled(StringKey("core"))
    core.inner = commentParser ~ core ~ "+" ~ core |
      core ~ "+" ~ core |
      commentParser ~ number

    val input = "/* jo */ 2 + 3"
    val result = TestGrammarUtils.parseAndPrint(input, None, core)
  }

  test("Plus") {
    lazy val commentParser: PackratParser[Any] = regex(new Regex( """/\*.*\*/"""))*
    lazy val parser: PackratParser[Any] =
      commentParser ~ parser ~ literal("+") ~ parser |||
        commentParser ~ wholeNumber
    val input = "/* jo */ 2 + 3"
    val reader = new PackratReader[Char](new CharArrayReader(input.toCharArray))
    val result = parser(reader)
  }

  test("PlusWithoutLazy") {
    val commentParser: PackratParser[Any] = regex(new Regex( """/\*.*\*/"""))*
    var parser: PackratParser[Any] = null
    parser = commentParser ~ parser ~ literal("+") ~ parser |||
        commentParser ~ wholeNumber
    val input = "/* jo */ 2 + 3"
    val reader = new PackratReader[Char](new CharArrayReader(input.toCharArray))
    val result = parser(reader)
  }

  test("PlusWithoutLazy2") {
    val commentParser: PackratParser[Any] = regex(new Regex( """/\*.*\*/"""))*
    var parser: PackratParser[Any] = null
    parser = commentParser ~ parser ~ literal("+") ~ parser |
      commentParser ~ wholeNumber
    val input = "/* jo */ 2 + 3"
    val reader = new PackratReader[Char](new CharArrayReader(input.toCharArray))
    val result = parser(reader)
  }

  test("EmptyComment") {
    val commentParser: BiGrammar = RegexG(new Regex("""/\*.*\*/""")).manyVertical
    val input = "/* jo */ 2 + 3"
    val result = TestGrammarUtils.parseAndPrint("", None, commentParser)
  }

  /*
  Curious that this test fails. Wouldn't expect it to. Especially since the next test succeeds.
   */
  ignore("MinusPlusRightCommutationBiGrammar") {
    val commentParser: BiGrammar = RegexG(new Regex("""/\*.*\*/""")).manyVertical
    val core = new Labelled(StringKey("core"))
    core.inner = commentParser ~ number | commentParser ~ core ~ "+" ~ core

    val input = "/* jo */ 2 + 3"
    val result = TestGrammarUtils.parseAndPrint(input, None, core)
  }

  test("MinusPlus") {
    lazy val commentParser: PackratParser[Any] = regex(new Regex( """/\*.*\*/"""))*
    lazy val parser: PackratParser[Any] =
          commentParser ~ parser ~ literal("-") ~ parser |||
          commentParser ~ parser ~ literal("+") ~ parser |||
          commentParser ~ wholeNumber
    val input = "/* jo */ 2 + 3"
    val reader = new PackratReader[Char](new CharArrayReader(input.toCharArray))
    val result = parser(reader)
  }

  test("MinusPlusRightCommutation") {
    lazy val commentParser: PackratParser[Any] = regex(new Regex( """/\*.*\*/"""))*
    lazy val withoutMinus: PackratParser[Any] =
        commentParser ~ withoutMinus ~ literal("+") ~ withoutMinus |||
        commentParser ~ wholeNumber

    lazy val parser: PackratParser[Any] = commentParser ~ parser ~ literal("-") ~ withoutMinus ||| withoutMinus
    val input = "/* jo */ 2 + 3"
    val reader = new PackratReader[Char](new CharArrayReader(input.toCharArray))
    val result = parser(reader)
  }

//  test("MinusPlusRightCommutation") {
//    lazy val commentParser: core.grammar.Grammar = core.grammar.Many(core.grammar.RegexG(new Regex("""/\*.*\*/""")))
//    lazy val withoutMinus: Grammar =
//      core.grammar.Choice(commentParser.~(withoutMinus).~(literal("+")).~(withoutMinus),
//        commentParser.~(wholeNumber))
//
//    lazy val parser: Grammar = commentParser ~ parser ~ literal("-") ~ withoutMinus ||| withoutMinus
//    val input = "/* jo */ 2 + 3"
//    val reader = new PackratReader[Char](new CharArrayReader(input.toCharArray))
//    val result = parser(reader)
//  }

  test("MinusPlusRightCommutationBiGrammar2") {
    val commentParser: BiGrammar = RegexG(new Regex("""/\*.*\*/""")).manyVertical
    val core = new Labelled(StringKey("core"))
    core.inner = commentParser ~ core ~ "+" ~ core |
      commentParser ~ number

    val parser = new Labelled(StringKey("parser"))
    parser.inner = core //commentParser ~ parser ~ "-" ~ core | core
    val input = "2 + 3"
    val result = TestGrammarUtils.parseAndPrint(input, None, core)
  }

  test("MinusPlusRightCommutationBiGrammar3") {
    val commentParser: BiGrammar = RegexG(new Regex("""/\*.*\*/""")).manyVertical
    val core = new Labelled(StringKey("core"))
    core.inner = core ~ "+" ~ core |
      commentParser ~ number

    val parser = new Labelled(StringKey("parser"))
    parser.inner = core //commentParser ~ parser ~ "-" ~ core | core
    val input = "/* jo */ 2 + 3"
    val result = TestGrammarUtils.parseAndPrint(input, None, core)
  }

  test("RightRecursionUsingBiGrammar") {
    val commentsGrammar: BiGrammar = JavaStyleCommentsC.CommentsGrammar
    val expression = new core.bigrammar.Labelled(StringKey("expression"))
    expression.addOption(commentsGrammar ~> number)
    expression.addOption(commentsGrammar ~> (expression ~~ "+" ~~ expression))
    val input = "/* jo */ 2 + 3"
    val result = TestGrammarUtils.parseAndPrint(input, None, expression)

    assertResult("2 + 3")(result)
  }

  test("RightRecursionUsingBiGrammarWithSubtraction") {
    val commentsGrammar: BiGrammar = JavaStyleCommentsC.CommentsGrammar
    val expression = new core.bigrammar.Labelled(StringKey("expression"))
    expression.addOption(commentsGrammar ~> number)
    expression.addOption(commentsGrammar ~> (expression ~~ "+" ~~ expression))
    val withoutSubtraction = expression.inner
    expression.addOption(commentsGrammar ~> (expression ~~ "-" ~~ withoutSubtraction))
    val input = "/* jo */ 2 + 3"
    val result = TestGrammarUtils.parseAndPrint(input, None, expression)

    assertResult("2 + 3")(result)
  }

  test("VerySimilarToCompilerAddSub") {
    val expression: Labelled = getManualAddSubGrammar
    val input = "/* jo */ 2 + 3"
    val result = TestGrammarUtils.parseAndPrint(input, None, expression)

    assertResult("2 + 3")(result)
  }

  /*
  Simpler reproduction of the problem, which I now work around by using parseMap instead of asNode in AdditionC.
   */
  ignore("VerySimilarToCompilerSubAdd") {
    val expression: Labelled = getManualSubAddGrammar
    val input = "/* jo */ 2 + 3"
    val result = TestGrammarUtils.parseAndPrint(input, None, expression)

    assertResult("~(~(2,+),3)")(result)
  }

  def getManualSubAddGrammar: Labelled = {
    val commentsGrammar: BiGrammar = JavaStyleCommentsC.CommentsGrammar
    val core = new Labelled(StringKey("core"))
    val addition = new Labelled(StringKey("addition"), core)
    val expression = new Labelled(StringKey("expression"), addition)
    core.addOption(commentsGrammar ~> number)
    val withoutSubtraction = expression.inner
    addition.addOption(commentsGrammar ~> (expression ~~ "-" ~~ withoutSubtraction))
    addition.addOption(commentsGrammar ~> (expression ~~ "+" ~~ expression))
    expression
  }

  def getManualAddSubGrammar: Labelled = {
    val commentsGrammar: BiGrammar = JavaStyleCommentsC.CommentsGrammar
    val core = new Labelled(StringKey("core"))
    val addition = new Labelled(StringKey("addition"), core)
    val expression = new Labelled(StringKey("expression"), addition)
    core.addOption(commentsGrammar ~> number)
    addition.addOption(commentsGrammar ~> (expression ~~ "+" ~~ expression))
    val withoutSubtraction = expression.inner
    addition.addOption(commentsGrammar ~> (expression ~~ "-" ~~ withoutSubtraction))
    expression
  }
}
