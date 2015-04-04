package transformations.javaPlus

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import transformations.javac.classes.skeleton.JavaClassSkeleton
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.methods.MethodC._
import transformations.javac.methods.{MethodC, ReturnExpressionC}

object ExpressionMethodC extends ParticleWithGrammar with ParticleWithPhase {

  override def dependencies: Set[Contract] = Set(ReturnExpressionC, MethodC, JavaClassSkeleton) ++ super.dependencies

  object ExpressionMethodKey
  object ExpressionMethodExpression

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val visibilityGrammar = grammars.find(MethodC.VisibilityGrammar)
    val parseStatic = grammars.find(MethodC.StaticGrammar)
    val parseReturnType = grammars.find(MethodC.ReturnTypeGrammar)
    val parseParameters = grammars.find(MethodC.ParametersGrammar)
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val expressionMethodGrammar = visibilityGrammar ~~ parseStatic ~~ parseReturnType ~~
      identifier ~ parseParameters ~~ ("=" ~~> expressionGrammar) ^^
      parseMap(ExpressionMethodKey, VisibilityKey, StaticKey, ReturnTypeKey, MethodNameKey,
        MethodParametersKey, ExpressionMethodExpression)
    val methodGrammar = grammars.find(MethodC.MethodGrammar)
    methodGrammar.addOption(expressionMethodGrammar)
  }

  override def transform(clazz: Node, state: CompilationState): Unit = {
    for(expressionMethod <- JavaClassSkeleton.getMembers(clazz).filter(method => method.clazz == ExpressionMethodKey))
    {
      val expression = expressionMethod(ExpressionMethodExpression).asInstanceOf[Node]
      expressionMethod.clazz = MethodC.MethodKey
      expressionMethod(MethodC.MethodBodyKey) = Seq(ReturnExpressionC._return(expression))
      expressionMethod.data.remove(ExpressionMethodExpression)
    }
  }

  override def description: String = "Allows method bodies to be defined using only an expression."
}
