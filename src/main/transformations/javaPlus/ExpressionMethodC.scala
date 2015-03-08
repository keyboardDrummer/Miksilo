package transformations.javaPlus

import core.transformation.grammars.GrammarCatalogue
import core.transformation._
import transformations.javac.classes.JavaClassSkeleton
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

  override def transform(clazz: MetaObject, state: CompilationState): Unit = {
    for(expressionMethod <- JavaClassSkeleton.getMembers(clazz).filter(method => method.clazz == ExpressionMethodKey))
    {
      val expression = expressionMethod(ExpressionMethodExpression).asInstanceOf[MetaObject]
      expressionMethod.clazz = MethodC.MethodKey
      expressionMethod(MethodC.MethodBodyKey) = Seq(ReturnExpressionC._return(expression))
    }
  }

  override def description: String = "Allows method bodies to be defined using only an expression."
}
