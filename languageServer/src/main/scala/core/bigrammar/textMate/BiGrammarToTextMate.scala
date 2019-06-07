package core.bigrammar.textMate

import core.bigrammar.BiGrammar
import core.bigrammar.grammars._
import core.language.node.Node
import deltas.expression.ArrayLiteralDelta
import deltas.expression.ArrayLiteralDelta.ArrayLiteral
import deltas.json.JsonObjectLiteralDelta.{ObjectLiteral, ObjectLiteralMember}
import deltas.json.{JsonObjectLiteralDelta, StringLiteralDelta}

object BiGrammarToTextMate {

  def toTextMate(grammar: BiGrammar): String = {
    val textMate: Node = createTextMateAstFromBiGrammar(grammar)
    printJson(textMate)
//    val textMateDeltas = JsonLanguage.deltas
//    val createTextMateLanguage = LanguageFromDeltas(Seq(PrettyPrint()) ++ textMateDeltas)
//    val compilation = createTextMateLanguage.compileAst(textMate)
//    compilation.output
  }

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
          indent()
          addNewLine()
          for(element <- array.members.dropRight(1)) {
            add(element)
            builder.append(",")
            addNewLine()
          }
          add(array.members.last)
          dedent()
          addNewLine()
          builder.append("]")

        case JsonObjectLiteralDelta.Shape =>
          val obj: ObjectLiteral[Node] = node
          builder.append("{")
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
          builder.append("}")

        case JsonObjectLiteralDelta.MemberShape =>
          val member: ObjectLiteralMember[Node] = node
          builder.append("\"" + member.key + "\"").append(": ")
          add(member.value)

        case StringLiteralDelta.Shape =>
          builder.append("\"" + StringLiteralDelta.getValue(node).replace("\\", "\\\\") + "\"")
      }
    }
    add(root)
    builder.toString()
  }

  def createTextMateAstFromBiGrammar(grammar: BiGrammar): Node = {
    val reachables = grammar.selfAndDescendants.toSet

    val patterns: Set[Node] = reachables.collect({
      case delimiter: Delimiter => TextMateDelta.singleMatch("keyword.operator", escapeRegex(delimiter.value).r)
      case keyword: Keyword /*if keyword.reserved*/ =>
        TextMateDelta.singleMatch("keyword.control", ("\\b" + escapeRegex(keyword.value) + "\\b").r)
      case _: Identifier => TextMateDelta.singleMatch("variable", """\b[A-Za-z][A-Za-z0-9_]*\b""".r)
      case NumberGrammar => TextMateDelta.singleMatch("constant.numeric", """-?\d+""".r)
      case StringLiteral => TextMateDelta.singleMatch("string.quoted", """"([^"\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*"""".r)
      case Colorize(RegexGrammar(regex, _), _, textMateScope) =>
        TextMateDelta.singleMatch(textMateScope, regex)
    })

    JsonObjectLiteralDelta.neww(Map(
      "patterns" -> ArrayLiteralDelta.create(patterns.toSeq)
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
