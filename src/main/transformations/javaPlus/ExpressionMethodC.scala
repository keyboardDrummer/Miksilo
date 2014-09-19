package transformations.javaPlus

import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.{GrammarTransformation, ProgramTransformation}
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton
import transformations.javac.classes.ClassC
import transformations.javac.expressions.ExpressionC
import transformations.javac.methods.MethodC._
import transformations.javac.methods.{MethodC, ReturnExpressionC}

object ExpressionMethodC extends GrammarTransformation with ProgramTransformation {

  override def dependencies: Set[Contract] = Set(ReturnExpressionC, MethodC, ClassC) ++ super.dependencies

  object ExpressionMethodKey
  object ExpressionMethodExpression

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val visibilityGrammar = grammars.find(MethodC.VisibilityGrammar)
    val parseStatic = grammars.find(MethodC.StaticGrammar)
    val parseReturnType = grammars.find(MethodC.ReturnTypeGrammar)
    val parseParameters = grammars.find(MethodC.ParametersGrammar)
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    val expressionMethodGrammar = visibilityGrammar ~~ parseStatic ~~ parseReturnType ~~
      identifier ~ parseParameters ~~ ("=" ~~> expressionGrammar) ^^
      parseMap(ExpressionMethodKey, VisibilityKey, StaticKey, ReturnTypeKey, MethodNameKey,
        MethodParametersKey, ExpressionMethodExpression)
    val methodGrammar = grammars.find(MethodC.MethodGrammar)
    methodGrammar.addOption(expressionMethodGrammar)
  }

  override def transform(clazz: MetaObject, state: TransformationState): Unit = {
    for(expressionMethod <- ClassC.getMethods(clazz).filter(method => method.clazz == ExpressionMethodKey))
    {
      val expression = expressionMethod(ExpressionMethodExpression).asInstanceOf[MetaObject]
      expressionMethod.clazz = ByteCodeSkeleton.MethodInfoKey
      expressionMethod(MethodC.MethodBodyKey) = Seq(ReturnExpressionC._return(expression))
    }
  }
}
