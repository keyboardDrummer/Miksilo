package deltas.javac.constructor

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.{ChildPath, NodePath, PathRoot}
import core.deltas.{Contract, DeltaWithGrammar, DeltaWithPhase}
import core.language.node.{Node, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.types.VoidTypeDelta
import deltas.expression.{ExpressionDelta, ExpressionInstance}
import deltas.javac.classes.ThisVariableDelta
import deltas.javac.classes.skeleton.JavaClassDelta._
import deltas.javac.methods.call.{CallDelta, CallNonVirtualDelta}

object ThisCallExpression extends DeltaWithPhase with DeltaWithGrammar with ExpressionInstance {

  override def description: String = "Enables calling a different constructor using 'this'"

  override val shape = ThisCall
  object ThisCall extends NodeShape

  def thisCall(arguments: Seq[Node]) = new Node(ThisCall, CallDelta.Arguments -> arguments)

  override def dependencies: Set[Contract] = Set(SuperCallExpressionDelta) ++ super.dependencies

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    PathRoot(program).visitShape(ThisCall, path => {
      val thisVariable = ThisVariableDelta.thisVariable
      val call = CallNonVirtualDelta.Shape.createWithData(
        Name -> SuperCallExpressionDelta.constructorName,
        CallDelta.Callee -> thisVariable,
        CallDelta.Arguments -> path.getFieldData(CallDelta.Arguments))
      path.asInstanceOf[ChildPath].replaceWith(call)
    })
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val callArguments = find(CallDelta.CallArgumentsGrammar)
    val thisCallGrammar = "this" ~> callArguments.as(CallDelta.Arguments) asNode ThisCall
    val expressionGrammar = find(ExpressionDelta.FirstPrecedenceGrammar)
    expressionGrammar.addAlternative(thisCallGrammar)
  }

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    builder.typesAreEqual(_type, VoidTypeDelta.constraintType)
  }
}
