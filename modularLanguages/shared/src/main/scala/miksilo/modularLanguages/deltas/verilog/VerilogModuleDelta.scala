package miksilo.modularLanguages.deltas.verilog

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.modularLanguages.core.node._
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.{ConstraintSkeleton, FileWithMembersDelta}
import miksilo.modularLanguages.deltas.HasNameDelta.HasName
import miksilo.modularLanguages.deltas.expression.VariableDelta
import miksilo.modularLanguages.deltas.expression.VariableDelta.Variable
import miksilo.modularLanguages.deltas.javac.classes.skeleton.HasConstraintsDelta

object VerilogModuleDelta extends DeltaWithGrammar with HasConstraintsDelta {

  import miksilo.modularLanguages.deltas.HasNameDelta.Name
  object Shape extends NodeShape

  object Ports extends NodeField
  object Body extends NodeField

  object MemberShape extends NodeShape

  def neww(name: String, ports: Seq[Node], body: Seq[Node]): Node = Shape.create(
    Name -> name,
    Ports -> ports,
    Body -> body)

  implicit class Module[T <: NodeLike](val node: T) extends NodeWrapper[T] with HasName[T] {
    def ports: Seq[Variable[T]] = NodeWrapper.wrapList(node(Ports).asInstanceOf[Seq[T]])
    def body: Seq[T] = node(Body).asInstanceOf[Seq[T]]
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val member = create(MemberShape)
    val variable = find(VariableDelta.Shape)
    val parameterList: BiGrammar = (variable.manySeparatedVertical(",").inParenthesis | value(Seq.empty)).as(Ports)
    val body: BiGrammar = member.manyVertical.as(Body)
    val moduleGrammar: BiGrammar = "module" ~~ find(Name) ~~ parameterList ~ ";" % body.indent() % "endmodule" asNode Shape
    find(FileWithMembersDelta.Members).addAlternative(moduleGrammar)
  }

  override def description: String = "Adds the Verilog module"

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val moduleScope = builder.newScope(parentScope, "moduleScope")
    val module: Module[NodePath] = path
    for(port <- module.ports) {
      builder.declare(port.name, moduleScope, port, None)
    }

    for(member <- module.body) {
      ConstraintSkeleton.constraints(compilation, builder, member, moduleScope)
    }
  }

  override def shape: NodeShape = Shape
  override def dependencies: Set[Contract] = Set(FileWithMembersDelta, VariableDelta)
}



