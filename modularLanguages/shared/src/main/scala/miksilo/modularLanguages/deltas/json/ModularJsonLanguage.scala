package miksilo.modularLanguages.deltas.json

import miksilo.modularLanguages.core.bigrammar.printer.BiGrammarToPrinter
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.PathRoot
import miksilo.modularLanguages.core.deltas.{Delta, LanguageFromDeltas, ParseUsingTextualGrammar}
import miksilo.languageServer.core.language.{Language, Phase}
import miksilo.modularLanguages.core.node.Node
import miksilo.editorParser.parsers.editorParsers.UntilTimeStopFunction
import miksilo.modularLanguages.deltas.expression.ArrayLiteralDelta.ArrayLiteral
import miksilo.modularLanguages.deltas.expression._
import miksilo.modularLanguages.deltas.javac.expressions.literals.BooleanLiteralDelta
import miksilo.modularLanguages.deltas.json.JsonObjectLiteralDelta.{ObjectLiteral, ObjectLiteralMember}

object ModularJsonLanguage {
  val deltas: Seq[Delta] = Seq[Delta](ExpressionLanguageDelta, BooleanLiteralDelta, JsonObjectLiteralDelta,
    ArrayLiteralDelta, SingleQuotedStringLiteralDelta, JsonStringLiteralDelta, IntLiteralDelta, ExpressionDelta)
  val language = LanguageFromDeltas(Seq(ParseUsingTextualGrammar(UntilTimeStopFunction(100))) ++ deltas)
}

object PrintJson extends Delta  {

  override def inject(language: Language): Unit = {
    language.compilerPhases = List(Phase(this, description, compilation => {
      try
        {
          compilation.output = JsonPrinter.printJson(compilation.program.asInstanceOf[PathRoot].current)
        } catch {
        case _: Throwable =>
      }
    }))
  }

  override def description = "Prints the current program as Json to the output"

  override def dependencies = Set.empty
}

object JsonPrinter {
  lazy val jsonPrinter = BiGrammarToPrinter.toPrinter(LanguageGrammars.grammars.get(ModularJsonLanguage.language).root)

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

        case StringLiteralDelta.Shape =>
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
}