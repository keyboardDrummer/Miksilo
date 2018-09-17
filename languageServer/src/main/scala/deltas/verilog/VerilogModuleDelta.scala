package deltas.verilog

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.{BodyGrammar, LanguageGrammars}
import core.deltas.path.{NodePath, PathRoot}
import core.language.Language
import core.language.node._
import deltas.ConstraintSkeleton
import deltas.expressions.VariableDelta
import deltas.expressions.VariableDelta.Variable

object VerilogModuleDelta extends DeltaWithGrammar {

  object Shape extends NodeShape
  object Name extends NodeField
  object Ports extends NodeField
  object Body extends NodeField

  object MemberShape extends NodeShape

  def neww(name: String, ports: Seq[Node], body: Seq[Node]): Node = Shape.create(
    Name -> name,
    Ports -> ports,
    Body -> body)

  implicit class Module[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def ports: Seq[Variable[T]] = NodeWrapper.wrapList(node(Ports).asInstanceOf[Seq[T]])
    def body: Seq[T] = node(Body).asInstanceOf[Seq[T]]
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val member = create(MemberShape)
    val variable = find(VariableDelta.VariableGrammar)
    val parameterList: BiGrammar = variable.manySeparatedVertical(",").as(Ports).inParenthesis
    val body: BiGrammar = member.manyVertical.as(Body)
    val moduleGrammar: BiGrammar = "module" ~~ identifier.as(Name) ~~ parameterList ~ ";" % body.indent() % "endmodule" asNode Shape
    find(BodyGrammar).inner = moduleGrammar
  }

  override def description: String = "Adds the Verilog module"

  override def inject(language: Language): Unit = {

    language.collectConstraints = (compilation, builder) => {
      val moduleScope = builder.newScope(None, "moduleScope")
      val module: Module[NodePath] = PathRoot(compilation.program)
      for(port <- module.ports) {
        builder.declare(port.name, moduleScope, port, None)
      }

      for(member <- module.body) {
        ConstraintSkeleton.constraints(compilation, builder, member, moduleScope)
      }
    }
    super.inject(language)
  }
}



