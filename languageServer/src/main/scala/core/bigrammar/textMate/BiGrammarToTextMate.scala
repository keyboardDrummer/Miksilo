package core.bigrammar.textMate

import _root_.core.bigrammar.grammars.{ParseWhiteSpace, _}
import core.bigrammar.{BiGrammar, BiGrammarToParser}
import core.bigrammar.printer.BiGrammarToPrinter
import core.language.node.Node
import deltas.expression.ArrayLiteralDelta.ArrayLiteral
import deltas.expression.{ArrayLiteralDelta, ExpressionDelta}
import deltas.json.JsonObjectLiteralDelta.{ObjectLiteral, ObjectLiteralMember}
import deltas.json.{JsonLanguage, JsonObjectLiteralDelta, JsonStringLiteralDelta}
import util.GraphBasics

import scala.util.matching.Regex

object BiGrammarToTextMate {

  def toTextMate(grammar: BiGrammar): String = {
    val textMate: Node = createTextMateAstFromBiGrammar(grammar)
    printJson(textMate)
  }

  // TODO let this operate on parsers instead of BiGrammar, that way less cases have to be handled.
  def grammarToRegex(root: BiGrammar): Option[String] = {
    var callStack = List.empty[BiGrammar]

    def recurse(grammar: BiGrammar): Option[String] = {
      if (callStack.contains(grammar))
        return None

      callStack ::= grammar

      val result: Option[String] = grammar match {
        case sequence: BiSequence =>
          for {
            left <- recurse(sequence.first)
            right <- recurse(sequence.second)
          } yield left + right
        case regex: RegexGrammar => Some(regex.regex.regex)
        case many: Many => recurse(many.inner).map(r => r + "*") // TODO add parenthesis
        case map: MapGrammar => recurse(map.inner)
        case map: MapGrammarWithMap => recurse(map.inner)
        case labelled: Labelled => recurse(labelled.inner)
        case delimiter: Delimiter => Some(escapeRegex(delimiter.value))
        case _: Identifier => Some(escapeRegex(BiGrammarToParser.identifier.regex.regex))
        case ParseWhiteSpace => Some(ParseWhiteSpace.regex.regex)
        case keyword: Keyword => Some(escapeRegex(keyword.value))
        case choice: BiChoice =>
          for {
            left <- recurse(choice.left)
            right <- recurse(choice.right)
          } yield left + "|" + right
        case as: As => recurse(as.inner)
      }

      callStack = callStack.tail
      result
    }

    recurse(root)
  }

  val jsonLanguage = JsonLanguage.language
  lazy val jsonPrinter = BiGrammarToPrinter.toPrinter(jsonLanguage.grammars.root)

  // TODO replace this with a JsonLanguage printer that actually works.
  def printJson(root: Node): String = {
    val builder = new StringBuilder
    var indentation = 0
    def add(node: Node): Unit = {
      def addNewLine() = builder.append("\n" + " " * indentation)
      def indent(): Unit = indentation += 2
      def dedent(): Unit = indentation -= 2

      node.shape match {
        case ArrayLiteralDelta.Shape =>
          val array: ArrayLiteral[Node] = node
          builder.append("[")
          if (array.members.nonEmpty) {
            indent()
            addNewLine()
            for(element <- array.members.dropRight(1)) {
              add(element)
              builder.append(",")
              addNewLine()
            }
            add(array.members.last)
            dedent()
          }
          addNewLine()
          builder.append("]")

        case JsonObjectLiteralDelta.Shape =>
          val obj: ObjectLiteral[Node] = node
          builder.append("{")
          if (obj.members.nonEmpty) {
            indent()
            addNewLine()
            for(member <- obj.members.dropRight(1)) {
              add(member)
              builder.append(",")
              addNewLine()
            }
            add(obj.members.last)
            dedent()
            addNewLine()
          }
          builder.append("}")

        case JsonObjectLiteralDelta.MemberShape =>
          val member: ObjectLiteralMember[Node] = node
          builder.append("\"" + member.key + "\"").append(": ")
          add(member.value)

        case JsonStringLiteralDelta.Shape =>
          val regex = JsonStringLiteralDelta.getValue(node)
          builder.append(stringToStringLiteral(regex))

        case ExpressionDelta.DefaultShape =>
          // Print nothing
        case _ =>
          builder.append(jsonPrinter(node))
      }
    }
    add(root)
    builder.toString()
  }

  private def stringToStringLiteral(value: String) = {
    "\"" + value.
      replace("\"", "\\\"").
      replace("\\", "\\\\") + "\""
  }

  case class Match(scope: String, regex: Regex)
  def createTextMateAstFromBiGrammar(grammar: BiGrammar): Node = {
    val reachables = GraphBasics.traverseBreadth[BiGrammar](Seq(grammar), grammar => grammar.children,
      node => if (node.isInstanceOf[Colorize]) GraphBasics.SkipChildren else GraphBasics.Continue )

    val typedPatterns: Seq[Match] = reachables.collect({
      case delimiter: Delimiter =>
        Match("keyword.operator", escapeRegex(delimiter.value).r)
      case keyword: Keyword /*if keyword.reserved*/ =>
        Match("keyword.control", ("\\b" + escapeRegex(keyword.value) + "\\b").r)
      case _: Identifier =>
        Match("variable", """\b[A-Za-z][A-Za-z0-9_]*\b""".r)
      case NumberGrammar =>
        Match("constant.numeric", """-?\d+""".r)
      case StringLiteral =>
        Match("string.quoted", """"([^"\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*"""".r)
      case Colorize(inner, textMateScope) =>
        val maybeRegex = grammarToRegex(inner)
        maybeRegex match {
          case None => throw new Exception("Colorize did not contain a regex")
          case Some(regex) =>
// TODO turn on validation, once we figure out how to filter out NOT \n, and ending in \n. For example the line comment regex currently incorrectly triggers this validation
//            if (regex.contains("\\n") || regex.contains("\\s"))
//              throw new Exception(s"Colorize regex $regex contained a newline")

            Match(textMateScope, regex.r)
        }
    }).sortBy(n => n.regex.regex)

    val patterns = typedPatterns.map(pattern => TextMateDelta.singleMatch(pattern.scope, pattern.regex)).distinct
    JsonObjectLiteralDelta.neww(Map(
      "patterns" -> ArrayLiteralDelta.create(patterns)
    ))
  }

  def escapeRegex(regex: String): String = {
    var result = regex
    val chars = "\\<([{^-=$!|]})?*+.>"
    for(char <- chars) {
      result = result.replace(char.toString, "\\" + char)
    }
    result
  }
}
