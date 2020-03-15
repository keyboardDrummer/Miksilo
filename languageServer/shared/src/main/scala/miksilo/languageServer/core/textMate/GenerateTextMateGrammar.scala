package miksilo.languageServer.core.textMate

import miksilo.editorParser.parsers.core.OptimizingParserWriter
import miksilo.editorParser.parsers.strings.CommonParserWriter
import util.GraphBasics

import scala.util.matching.Regex

object GenerateTextMateGrammar {

  def toTextMate(writer: CommonParserWriter)(grammar: writer.Parser[_]): String = {
    val textMate: JsExpression = createTextMateAstFromBiGrammar(writer)(grammar)
    JsonPrinter.printJson(textMate)
  }

  def grammarToRegex[Result](writer: CommonParserWriter)(root: writer.Parser[Result]): Option[String] = {
    var callStack = List.empty[writer.Parser[_]]

    def recurse(grammar: writer.Parser[_]): Option[String] = {
      if (callStack.contains(grammar))
        return None

      callStack ::= grammar

      val result: Option[String] = grammar match {
        case sequence: writer.SequenceLike[_] =>
          for {
            left <- recurse(sequence.left)
            right <- recurse(sequence.right)
          } yield left + right
        case choice: writer.ChoiceLike[_] =>
          for {
            left <- recurse(choice.first)
            right <- recurse(choice.second)
          } yield left + "|" + right
        case regex: writer.RegexParser => Some(regex.regex.regex)
        case delimiter: writer.Literal => Some(escapeLiteral(delimiter.value))
        case keyword: writer.KeywordParser => Some("\\b" + escapeLiteral(keyword.value) + "\\b")
        // TODO how to incorporate many? case many: Many => recurse(many.inner).map(r => r + "*") // TODO add parenthesis
        case wrapper: writer.ParserWrapper[_] => recurse(wrapper.original)
      }

      callStack = callStack.tail
      result
    }

    recurse(root)
  }

  case class Match(scope: String, regex: Regex)

  def createTextMateAstFromBiGrammar(writer: CommonParserWriter)(grammar: writer.Parser[_]): JsExpression = {
    val reachables: Seq[writer.Parser[_]] = GraphBasics.traverseBreadth[writer.Parser[_]](Seq(grammar), grammar => grammar.children,
      node => if (node.isInstanceOf[ColorizeLike] || node.isInstanceOf[writer.KeywordParser] ||
        node == writer.parseIdentifier || node == writer.stringLiteral) GraphBasics.SkipChildren else GraphBasics.Continue )

    val typedPatterns: Seq[Match] = reachables.collect({
      case parser if parser == writer.parseIdentifier =>
        Match("variable", """\b[A-Za-z][A-Za-z0-9_]*\b""".r)
      case regexParser: writer.RegexParser if Set("decimal number","floating point number","whole number").contains(regexParser.regexName) =>
        Match("constant.numeric", """-?\d+""".r)
      case keyword: writer.KeywordParser /*if keyword.reserved*/ =>
        Match("keyword.control", grammarToRegex(writer)(keyword).get.r)
      case delimiter: writer.Literal =>
        Match("keyword.operator", escapeLiteral(delimiter.value).r)
      case parser if parser == writer.stringLiteral =>
        Match("string.quoted.double", grammarToRegex(writer)(parser).get.r)
      case colorizeLike: ColorizeLike =>
        val maybeRegex = colorizeLike.getRegex
        maybeRegex match {
          case None => throw new Exception("Colorize did not contain a regex")
          case Some(regex) =>
            // TODO turn on validation, once we figure out how to filter out NOT \n, and ending in \n. For example the line comment regex currently incorrectly triggers this validation
            //            if (regex.contains("\\n") || regex.contains("\\s"))
            //              throw new Exception(s"Colorize regex $regex contained a newline")

            Match(colorizeLike.textMateScope, regex.r)
        }
    }).sortBy(n => n.regex.regex)

    val patterns = typedPatterns.map(pattern => singleMatch(pattern.scope, pattern.regex)).distinct
    JsObject(Map(
      "patterns" -> JsArray(patterns)
    ))
  }

  def singleMatch(name: String, regex: Regex): JsExpression = {
    JsObject(Map(
      "name" -> JsLiteral(name),
      "match" -> JsLiteral(regex.toString())))
  }

  def escapeLiteral(regex: String): String = {
    var result = regex
    val chars = "\\<([{^-=$!|]})?*+.>"
    for(char <- chars) {
      result = result.replace(char.toString, "\\" + char)
    }
    result
  }
}

trait ColorizeLike {
  def textMateScope: String
  def getRegex: Option[String]
}

trait TextMateGeneratingParserWriter extends CommonParserWriter with OptimizingParserWriter {

  case class Colorize[Result](original: Parser[Result], textMateScope: String) extends ParserBuilderBase[Result]
    with ParserWrapper[Result] with ColorizeLike {
    override def getParser(recursive: GetParser) = recursive(original)

    override def getRegex: Option[String] = {
      GenerateTextMateGrammar.grammarToRegex(TextMateGeneratingParserWriter.this)(original)
    }
  }
}
