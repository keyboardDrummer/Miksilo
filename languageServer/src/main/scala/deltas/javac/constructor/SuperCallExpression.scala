package deltas.javac.constructor

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.node.{Node, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.objects.Reference
import core.smarts.scopes.ReferenceInScope
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.types.VoidTypeDelta
import deltas.expression.{ExpressionDelta, ExpressionInstance}
import deltas.javac.classes.skeleton.JavaClassDelta
import deltas.javac.classes.skeleton.JavaClassDelta._
import deltas.javac.methods.MethodDelta
import deltas.javac.methods.MethodDelta.Method
import deltas.javac.methods.call.CallDelta
import deltas.javac.methods.call.CallDelta.Call

object SuperCallExpression extends DeltaWithGrammar with ExpressionInstance {

  override def description: String = "Enables calling a super constructor."

  override val shape = Shape

  override def dependencies: Set[Contract] = Set(MethodDelta, JavaClassDelta, ExpressionDelta) ++ super.dependencies

  def superCall(arguments: Seq[Node] = Seq()) = new Node(Shape, CallDelta.Arguments -> arguments)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val callArguments = find(CallDelta.CallArgumentsGrammar)
    val superCallGrammar = "super" ~> callArguments.as(CallDelta.Arguments) asNode Shape
    val expressionGrammar = find(ExpressionDelta.FirstPrecedenceGrammar)
    expressionGrammar.addAlternative(superCallGrammar)
  }

  object Shape extends NodeShape

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, call: NodePath, _type: Type, parentScope: Scope): Unit = {
    val method: Method[NodePath] = call.findAncestorShape(MethodDelta.Shape)
    val clazz: JavaClass[NodePath] = call.findAncestorShape(JavaClassDelta.Shape)
    val parentName = clazz.parent.get
    val superClass = builder.resolve(parentName, call.getField(ClassParent), parentScope)
    val superScope = builder.getDeclaredScope(superClass)

    val superReference = new Reference(method.name, Some(call))
    builder.add(ReferenceInScope(superReference, superScope))
    CallDelta.callConstraints(compilation, builder, call.arguments, parentScope, superReference, VoidTypeDelta.constraintType)
    builder.typesAreEqual(_type, VoidTypeDelta.constraintType) // TODO this is incorrect for the non-constructor case.
  }
}
