package deltas.javac.constructor

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.{ChildPath, NodePath, PathRoot}
import core.deltas.{Contract, DeltaWithGrammar, DeltaWithPhase}
import core.language.node.{Node, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.objects.Reference
import core.smarts.scopes.ReferenceInScope
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.types.VoidTypeDelta
import deltas.expression.{ExpressionDelta, ExpressionInstance}
import deltas.javac.classes.ThisVariableDelta
import deltas.javac.classes.skeleton.JavaClassDelta
import deltas.javac.classes.skeleton.JavaClassDelta._
import deltas.javac.methods.MemberSelectorDelta
import deltas.javac.methods.call.CallDelta.Call
import deltas.javac.methods.call.{CallDelta, CallNonVirtualDelta, CallStaticOrInstanceDelta}

object SuperCallExpressionDelta extends DeltaWithPhase with DeltaWithGrammar with ExpressionInstance {

  override def description: String = "Enables calling a super constructor."

  override val shape = SuperCall
  val constructorName: String = "<init>"

  override def dependencies: Set[Contract] = Set(CallStaticOrInstanceDelta) ++ super.dependencies

  def superCall(arguments: Seq[Node] = Seq()) = new Node(SuperCall, CallDelta.Arguments -> arguments)

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    PathRoot(program).visitShape(SuperCall, path => {
      val callee = MemberSelectorDelta.neww(ThisVariableDelta.thisVariable, constructorName)
      val call = CallNonVirtualDelta.Shape.createWithData(
        CallDelta.Callee -> callee,
        CallDelta.Arguments -> path.getFieldData(CallDelta.Arguments))
      path.asInstanceOf[ChildPath].replaceWith(call)
    })
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val callArguments = find(CallDelta.CallArgumentsGrammar)
    val superCallGrammar = "super" ~> callArguments.as(CallDelta.Arguments) asNode SuperCall
    val expressionGrammar = find(ExpressionDelta.FirstPrecedenceGrammar)
    expressionGrammar.addAlternative(superCallGrammar)
  }

  object SuperCall extends NodeShape

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, call: NodePath, _type: Type, parentScope: Scope): Unit = {
    val clazz: JavaClass[NodePath] = call.findAncestorShape(JavaClassDelta.Shape)
    val parentName = clazz.parent.get
    val superClass = builder.resolve(parentName, call.getSourceElement(ClassParent), parentScope)
    val superScope = builder.getDeclaredScope(superClass)

    val superReference = new Reference(constructorName, Some(call))
    builder.add(ReferenceInScope(superReference, superScope))
    CallDelta.callConstraints(compilation, builder, call.arguments, parentScope, superReference, VoidTypeDelta.constraintType)
    builder.typesAreEqual(_type, VoidTypeDelta.constraintType)
  }
}
