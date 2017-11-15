package deltas.javaPlus

import core.particles._
import core.particles.grammars.LanguageGrammars
import core.particles.node.{Node, NodeClass, NodeField}
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.classes.skeleton.JavaClassSkeleton._
import deltas.javac.expressions.ExpressionSkeleton
import deltas.javac.methods.MethodDelta._
import deltas.javac.methods.{MethodDelta, ReturnExpressionC}

object ExpressionMethodC extends DeltaWithGrammar with DeltaWithPhase {

  override def dependencies: Set[Contract] = Set(ReturnExpressionC, MethodDelta, JavaClassSkeleton) ++ super.dependencies

  object ExpressionMethodKey extends NodeClass
  object ExpressionMethodExpression extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val visibilityGrammar = find(MethodDelta.VisibilityGrammar).as(VisibilityKey)
    val parseStatic = find(MethodDelta.StaticGrammar).as(StaticKey)
    val parseReturnType = find(MethodDelta.ReturnTypeGrammar).as(ReturnTypeKey)
    val parseParameters = find(MethodDelta.ParametersGrammar).as(MethodParametersKey)
    val expressionGrammar = find(ExpressionSkeleton.ExpressionGrammar).as(ExpressionMethodExpression)
    val expressionMethodGrammar = (visibilityGrammar ~~ parseStatic ~~ parseReturnType ~~
      identifier.as(MethodNameKey) ~ parseParameters ~~ ("=" ~~> expressionGrammar)).
      asNode(ExpressionMethodKey)
    val methodGrammar = find(MethodDelta.MethodGrammar)
    methodGrammar.addOption(expressionMethodGrammar)
  }

  override def transform(clazz: Node, state: Compilation): Unit = {
    for(expressionMethod <- clazz.members.filter(method => method.clazz == ExpressionMethodKey))
    {
      val expression = expressionMethod(ExpressionMethodExpression).asInstanceOf[Node]
      expressionMethod.clazz = MethodDelta.MethodKey
      expressionMethod(MethodDelta.MethodBodyKey) = Seq(ReturnExpressionC._return(expression))
      expressionMethod.data.remove(ExpressionMethodExpression)
    }
  }

  override def description: String = "Allows method bodies to be defined using only an expression."
}
