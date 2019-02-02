package deltas.expression

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.objects.Reference
import core.smarts.scopes.ReferenceInScope
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.types.{TypeSkeleton, UnqualifiedObjectTypeDelta, VoidTypeDelta}
import deltas.javac.constructor.ConstructorDelta
import deltas.javac.methods.call.CallDelta
import deltas.javac.methods.call.CallDelta.Arguments

object NewDelta extends DeltaWithGrammar with ExpressionInstance {

  override def description: String = "Enables using the new keyword to create a new object."

  object Shape extends NodeShape
  object Type extends NodeField

  implicit class NewCall[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def _type: T = node(Type).asInstanceOf[T]
    def arguments: Seq[T] = NodeWrapper.wrapList(node(Arguments).asInstanceOf[Seq[T]])
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

  override val shape = Shape

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    val call: NewCall[NodePath] = expression
    val classType = TypeSkeleton.getType(compilation, builder, call._type, parentScope)
    val classDeclaration = builder.getDeclarationOfType(classType)
    builder.typesAreEqual(classType, _type)
    val classScope = builder.getDeclaredScope(classDeclaration)

    val constructorReference = new Reference(ConstructorDelta.constructorName, Some(call))
    builder.add(ReferenceInScope(constructorReference, classScope))
    CallDelta.callConstraints(compilation, builder, call.arguments, parentScope, constructorReference, VoidTypeDelta.constraintType)
  }
}
