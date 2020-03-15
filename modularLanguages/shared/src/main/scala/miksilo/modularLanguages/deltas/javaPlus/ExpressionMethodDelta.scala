package miksilo.modularLanguages.deltas.javaPlus

import miksilo.modularLanguages.core.deltas._
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.node.{Node, NodeField, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.classes.ClassDelta.JavaClass
import miksilo.modularLanguages.deltas.expression.ExpressionDelta
import miksilo.modularLanguages.deltas.javac.classes.skeleton.JavaClassDelta
import miksilo.modularLanguages.deltas.javac.methods.MethodDelta.ReturnType
import miksilo.modularLanguages.deltas.javac.methods.{AccessibilityFieldsDelta, MethodDelta, ReturnExpressionDelta}
import miksilo.modularLanguages.deltas.statement.BlockDelta

object ExpressionMethodDelta extends DeltaWithGrammar with DeltaWithPhase {

  override def dependencies: Set[Contract] = Set(ReturnExpressionDelta, MethodDelta, JavaClassDelta)

  import miksilo.modularLanguages.deltas.HasNameDelta.Name

  object Shape extends NodeShape
  object Expression extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val visibilityGrammar = find(AccessibilityFieldsDelta.VisibilityField)
    val parseStatic = find(AccessibilityFieldsDelta.Static)
    val parseReturnType = find(MethodDelta.ReturnTypeGrammar).as(ReturnType)
    val parseParameters = find(MethodDelta.Parameters).as(MethodDelta.Parameters)
    val expressionGrammar = find(ExpressionDelta.FirstPrecedenceGrammar).as(Expression)
    val expressionMethodGrammar = (visibilityGrammar ~~ parseStatic ~~ parseReturnType ~~
      identifier.as(Name) ~ parseParameters ~~ ("=" ~~> expressionGrammar)).
      asNode(Shape)
    val methodGrammar = find(MethodDelta.Shape)
    methodGrammar.addAlternative(expressionMethodGrammar)
  }

  override def transformProgram(program: Node, state: Compilation): Unit = {
    val clazz: JavaClass[Node] = program
    for(expressionMethod <- clazz.members.filter(method => method.shape == Shape))
    {
      val expression = expressionMethod(Expression).asInstanceOf[Node]
      expressionMethod.shape = MethodDelta.Shape
      expressionMethod(MethodDelta.Body) = BlockDelta.neww(Seq(ReturnExpressionDelta.neww(expression)))
      expressionMethod.data.remove(Expression)
    }
  }

  override def description: String = "Allows method bodies to be defined using only an expression."
}
