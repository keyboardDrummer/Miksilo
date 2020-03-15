package miksilo.modularLanguages.deltas.javac.constructor

import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.modularLanguages.core.node.{Node, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.objects.Reference
import miksilo.languageServer.core.smarts.scopes.ReferenceInScope
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.Type
import miksilo.modularLanguages.deltas.bytecode.types.VoidTypeDelta
import miksilo.modularLanguages.deltas.classes.ClassDelta
import miksilo.modularLanguages.deltas.classes.ClassDelta.{ClassParent, JavaClass}
import miksilo.modularLanguages.deltas.expression.{ExpressionDelta, ExpressionInstance}
import miksilo.modularLanguages.deltas.javac.methods.MethodDelta
import miksilo.modularLanguages.deltas.javac.methods.MethodDelta.Method
import miksilo.modularLanguages.deltas.javac.methods.call.CallDelta
import miksilo.modularLanguages.deltas.javac.methods.call.CallDelta.Call

object SuperCallExpression extends DeltaWithGrammar with ExpressionInstance {

  override def description: String = "Enables calling a super constructor."

  override val shape = Shape

  override def dependencies: Set[Contract] = Set(MethodDelta, ExpressionDelta) ++ super.dependencies

  def superCall(arguments: Seq[Node] = Seq()) = new Node(Shape, CallDelta.Arguments -> arguments)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val callArguments = find(CallDelta.CallArgumentsGrammar)
    val superCallGrammar = "super" ~> callArguments.as(CallDelta.Arguments) asNode Shape
    val expressionGrammar = find(ExpressionDelta.FirstPrecedenceGrammar)
    expressionGrammar.addAlternative(superCallGrammar)
  }

  object Shape extends NodeShape

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, _call: NodePath, _type: Type, parentScope: Scope): Unit = {
    val call: Call[NodePath] = _call
    val method: Method[NodePath] = _call.findAncestorShape(MethodDelta.Shape)
    val clazz: JavaClass[NodePath] = _call.findAncestorShape(ClassDelta.Shape)
    val parentName = clazz.parent.get
    val superClass = builder.resolve(parentName, parentScope, _call.getField(ClassParent))
    val superScope = builder.getDeclaredScope(superClass)

    val superReference = new Reference(method.name, Some(_call))
    builder.add(ReferenceInScope(superReference, superScope))
    CallDelta.callConstraints(compilation, builder, call.arguments, parentScope, superReference, VoidTypeDelta.constraintType)
    builder.typesAreEqual(_type, VoidTypeDelta.constraintType) // TODO this is incorrect for the non-constructor case.
  }
}
