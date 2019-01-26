package deltas.javac.constructor

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.node.{Node, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.types.VoidTypeDelta
import deltas.expression.{ExpressionDelta, ExpressionInstance}
import deltas.javac.classes.skeleton.JavaClassDelta
import deltas.javac.classes.skeleton.JavaClassDelta._
import deltas.javac.expressions.ConvertsToByteCodeDelta
import deltas.javac.methods.call.CallDelta

object ThisCallExpression extends DeltaWithGrammar with ExpressionInstance with ConvertsToByteCodeDelta {

  override def description: String = "Enables calling a different constructor using 'this'"

  override val shape = ThisCall
  object ThisCall extends NodeShape

  def thisCall(arguments: Seq[Node]) = new Node(ThisCall, CallDelta.Arguments -> arguments)

  override def dependencies: Set[Contract] = Set(SuperCallExpression) ++ super.dependencies

  override def toByteCode(call: NodePath, compilation: Compilation): Seq[Node] = {
    val classCompiler = JavaClassDelta.getClassCompiler(compilation)
    transformThisCall(classCompiler.currentClass, call, compilation)
  }

  def transformThisCall(program: Node, call: NodePath, compilation: Compilation): Seq[Node] = {
    SuperCallExpression.transformToByteCode(call, compilation, program.name)
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
