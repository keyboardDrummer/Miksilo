package miksilo.modularLanguages.deltas.verilog

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.core.node._
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.expression.VariableDelta
import miksilo.modularLanguages.deltas.expression.VariableDelta.Variable
import miksilo.modularLanguages.deltas.javac.classes.skeleton.HasConstraintsDelta

object PortTypeSpecifierDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object Shape extends NodeShape
  object Variables extends NodeField
  object Type extends NodeField

  implicit class PortTypeSpecifier[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def variables: Seq[Variable[T]] = NodeWrapper.wrapList(node(Variables).asInstanceOf[Seq[T]])
  }

  def neww(_type: String, variables: Seq[Node]): Node = Shape.create(Type -> _type, Variables -> variables)

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val member = find(VerilogModuleDelta.MemberShape)

    val variable = find(VariableDelta.Shape)
    val _type: BiGrammar = "input" | "output" | "reg"
    val input: BiGrammar = _type.as(Type) ~~ variable.manySeparated(",").as(Variables) ~ ";" asNode Shape
    member.addAlternative(input)
  }

  override def description: String = "Adds the input/output/reg members"

  override def shape: NodeShape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val portTypeSpecifier: PortTypeSpecifier[NodePath] = path
    for(variable <- portTypeSpecifier.variables) {
      builder.resolve(variable.name, parentScope, variable)
    }
  }

  override def dependencies: Set[Contract] = Set(VerilogModuleDelta)
}
