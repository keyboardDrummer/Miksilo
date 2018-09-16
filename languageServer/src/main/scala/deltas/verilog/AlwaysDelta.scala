package deltas.verilog

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.ValueGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.deltas.{DeltaWithGrammar, NodeGrammarWriter}
import core.language.{Compilation, Language}
import core.language.node.{Node, NodeField, NodeShape}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.expressions.VariableDelta
import deltas.javac.classes.skeleton.HasConstraintsDelta
import deltas.statement.StatementDelta
import deltas.verilog.AlwaysDelta.SensitivityVariables

object AlwaysDelta extends DeltaWithGrammar with HasConstraintsDelta {
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

  override def shape: NodeShape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {

  }
}

object SensitivityVariable extends NodeGrammarWriter {
  object Shape extends NodeShape
  object Edge extends NodeField
  object Name extends NodeField

  def neww(edge: String, name: Node): Node = Shape.create(Edge -> edge, Name -> name)

  def getListGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._

    val edge: BiGrammar = "posedge" | "negedge" | ValueGrammar("")
    val variable = find(VariableDelta.VariableGrammar)
    val element: BiGrammar = edge.as(SensitivityVariable.Edge) ~~ variable.as(SensitivityVariable.Name) asNode SensitivityVariable.Shape
    val sensitivityList: BiGrammar = "@" ~ element.manySeparated("," | "or").as(SensitivityVariables).inParenthesis
    sensitivityList
  }
}
