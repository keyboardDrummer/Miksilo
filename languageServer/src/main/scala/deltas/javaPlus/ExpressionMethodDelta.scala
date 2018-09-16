package deltas.javaPlus

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.node.{Node, NodeField, NodeShape}
import core.language.{Compilation, Language}
import deltas.expressions.ExpressionDelta
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.classes.skeleton.JavaClassSkeleton._
import deltas.javac.methods.MethodDelta.{Name, _}
import deltas.javac.methods.{AccessibilityFieldsDelta, MethodDelta, ReturnExpressionDelta}
import deltas.statement.BlockDelta

object ExpressionMethodDelta extends DeltaWithGrammar with DeltaWithPhase {

  override def dependencies: Set[Contract] = Set(ReturnExpressionDelta, MethodDelta, JavaClassSkeleton)

  object Shape extends NodeShape
  object Expression extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val visibilityGrammar = find(AccessibilityFieldsDelta.VisibilityField)
    val parseStatic = find(AccessibilityFieldsDelta.Static)
    val parseReturnType = find(MethodDelta.ReturnTypeGrammar).as(ReturnType)
    val parseParameters = find(MethodDelta.ParametersGrammar).as(Parameters)
    val expressionGrammar = find(ExpressionDelta.FirstPrecedenceGrammar).as(Expression)
    val expressionMethodGrammar = (visibilityGrammar ~~ parseStatic ~~ parseReturnType ~~
      identifier.as(Name) ~ parseParameters ~~ ("=" ~~> expressionGrammar)).
      asNode(Shape)
    val methodGrammar = find(MethodDelta.MethodGrammar)
    methodGrammar.addAlternative(expressionMethodGrammar)
  }

  override def transformProgram(program: Node, state: Compilation): Unit = {
    for(expressionMethod <- program.members.filter(method => method.shape == Shape))
    {
      val expression = expressionMethod(Expression).asInstanceOf[Node]
      expressionMethod.shape = MethodDelta.Shape
      expressionMethod(MethodDelta.Body) = BlockDelta.neww(Seq(ReturnExpressionDelta._return(expression)))
      expressionMethod.data.remove(Expression)
    }
  }

  override def description: String = "Allows method bodies to be defined using only an expression."
}
