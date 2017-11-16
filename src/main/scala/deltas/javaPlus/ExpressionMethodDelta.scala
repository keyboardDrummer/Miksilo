package deltas.javaPlus

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeClass, NodeField}
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.classes.skeleton.JavaClassSkeleton._
import deltas.javac.expressions.ExpressionSkeleton
import deltas.javac.methods.MethodDelta._
import deltas.javac.methods.{AccessibilityFieldsDelta, MethodDelta, ReturnExpressionDelta}

object ExpressionMethodDelta extends DeltaWithGrammar with DeltaWithPhase {

  override def dependencies: Set[Contract] = Set(ReturnExpressionDelta, MethodDelta, JavaClassSkeleton) ++ super.dependencies

  object Clazz extends NodeClass
  object Expression extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val visibilityGrammar = find(AccessibilityFieldsDelta.VisibilityField)
    val parseStatic = find(AccessibilityFieldsDelta.Static)
    val parseReturnType = find(MethodDelta.ReturnTypeGrammar).as(ReturnTypeKey)
    val parseParameters = find(MethodDelta.ParametersGrammar).as(MethodParametersKey)
    val expressionGrammar = find(ExpressionSkeleton.ExpressionGrammar).as(Expression)
    val expressionMethodGrammar = (visibilityGrammar ~~ parseStatic ~~ parseReturnType ~~
      identifier.as(MethodNameKey) ~ parseParameters ~~ ("=" ~~> expressionGrammar)).
      asNode(Clazz)
    val methodGrammar = find(MethodDelta.MethodGrammar)
    methodGrammar.addOption(expressionMethodGrammar)
  }

  override def transform(clazz: Node, state: Compilation): Unit = {
    for(expressionMethod <- clazz.members.filter(method => method.clazz == Clazz))
    {
      val expression = expressionMethod(Expression).asInstanceOf[Node]
      expressionMethod.clazz = MethodDelta.Clazz
      expressionMethod(MethodDelta.Body) = Seq(ReturnExpressionDelta._return(expression))
      expressionMethod.data.remove(Expression)
    }
  }

  override def description: String = "Allows method bodies to be defined using only an expression."
}
