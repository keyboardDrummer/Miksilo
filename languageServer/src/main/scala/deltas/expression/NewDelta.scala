package deltas.expression

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.{ChildPath, PathRoot}
import core.deltas.{Contract, DeltaWithGrammar, DeltaWithPhase}
import core.language.node._
import core.language.{Compilation, Language}
import deltas.bytecode.types.UnqualifiedObjectTypeDelta
import deltas.javac.classes.ThisVariableDelta
import deltas.javac.constructor.SuperCallExpressionDelta.constructorName
import deltas.javac.methods.MemberSelectorDelta
import deltas.javac.methods.call.CallDelta.Arguments
import deltas.javac.methods.call.{CallDelta, CallNonVirtualDelta}

object NewDelta extends DeltaWithPhase with DeltaWithGrammar {

  override def description: String = "Enables using the new keyword to create a new object."

  object Shape extends NodeShape
  object Type extends NodeField

  implicit class NewCall[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def _type: T = node(Type).asInstanceOf[T]
    def arguments: Seq[T] = NodeWrapper.wrapList(node(Arguments).asInstanceOf[Seq[T]])
  }

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    PathRoot(program).visitShape(Shape, path => {
      val callee = MemberSelectorDelta.neww(ThisVariableDelta.thisVariable, constructorName)
      val call = CallNonVirtualDelta.Shape.createWithData(
        CallDelta.Callee -> callee,
        CallDelta.Arguments -> path.getFieldData(CallDelta.Arguments))
      path.asInstanceOf[ChildPath].replaceWith(call)
    })
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._

    val objectGrammar = find(UnqualifiedObjectTypeDelta.AnyObjectTypeGrammar)
    val callArgumentsGrammar = find(CallDelta.CallArgumentsGrammar)
    val newGrammar = "new" ~~> objectGrammar.as(Type) ~ callArgumentsGrammar.as(CallDelta.Arguments) asNode Shape
    val expressionGrammar = find(ExpressionDelta.LastPrecedenceGrammar)
    expressionGrammar.addAlternative(newGrammar)
  }

  override def dependencies: Set[Contract] = Set(ExpressionDelta, CallDelta)
}
