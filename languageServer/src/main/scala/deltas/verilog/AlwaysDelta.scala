package deltas.verilog

import core.bigrammar.BiGrammar
import core.deltas.{Contract, DeltaWithGrammar}
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.ConstraintSkeleton
import deltas.javac.classes.skeleton.HasConstraintsDelta
import deltas.statement.StatementDelta
import deltas.verilog.SensitivityVariableDelta.SensitivityVariable

object AlwaysDelta extends DeltaWithGrammar with HasConstraintsDelta {
  object Shape extends NodeShape
  object SensitivityVariables extends NodeField
  object Body extends NodeField

  def neww(variables: Seq[Node], body: Node): Node = Shape.create(SensitivityVariables -> variables, Body -> body)

  implicit class Always[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def variables: Seq[SensitivityVariable[T]] = NodeWrapper.wrapList(node(SensitivityVariables).asInstanceOf[Seq[T]])
    def body: T = node(Body).asInstanceOf[T]
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    val _grammar = grammars
    import grammars._

    val member = find(VerilogModuleDelta.MemberShape)
    val statement = find(StatementDelta.Grammar)
    val always: BiGrammar = "always" ~~ SensitivityVariableDelta.getListGrammar(_grammar) % statement.as(Body) asNode Shape
    member.addAlternative(always)
  }

  override def description: String = "Adds the always statement"

  override def shape: NodeShape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val always: Always[NodePath] = path
    for(variable <- always.variables) {
      builder.resolve(variable.name, variable.getLocation(SensitivityVariableDelta.Name), parentScope)
    }
    ConstraintSkeleton.constraints(compilation, builder, always.body, parentScope)
  }

  override def dependencies: Set[Contract] = Set(StatementDelta, VerilogModuleDelta)
}


