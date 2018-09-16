package deltas.verilog

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.ValueGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.{DeltaWithGrammar, NodeGrammarWriter}
import core.language.Language
import core.language.node.{Node, NodeField, NodeShape}
import deltas.statement.StatementDelta
import deltas.verilog.AlwaysDelta.SensitivityVariables

object AlwaysDelta extends DeltaWithGrammar {
  object Shape extends NodeShape
  object SensitivityVariables extends NodeField
  object Body extends NodeField

  def neww(variables: Seq[Node], body: Node): Node = Shape.create(SensitivityVariables -> variables, Body -> body)

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    val _grammar = grammars
    import grammars._

    val member = find(VerilogModuleDelta.MemberShape)
    val statement = find(StatementDelta.Grammar)
    val always: BiGrammar = "always" ~~ SensitivityVariable.getListGrammar(_grammar) % statement.as(Body) asNode Shape
    member.addAlternative(always)
  }

  override def description: String = "Adds the always statement"
}

object SensitivityVariable extends NodeGrammarWriter {
  object Shape extends NodeShape
  object Edge extends NodeField
  object Name extends NodeField

  def neww(edge: String, name: String): Node = Shape.create(Edge -> edge, Name -> name)

  def getListGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._

    val edge: BiGrammar = "posedge" | "negedge" | ValueGrammar("")
    val variable: BiGrammar = edge.as(SensitivityVariable.Edge) ~~ identifier.as(SensitivityVariable.Name) asNode SensitivityVariable.Shape
    val sensitivityList: BiGrammar = "@" ~ variable.manySeparated("," | "or").as(SensitivityVariables).inParenthesis
    sensitivityList
  }
}
