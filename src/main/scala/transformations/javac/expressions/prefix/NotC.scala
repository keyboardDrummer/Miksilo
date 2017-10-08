package transformations.javac.expressions.prefix

import core.particles.{Compilation, Language}
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField}
import core.particles.path.Path
import transformations.bytecode.extraBooleanInstructions.NotInstructionC
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.javac.types.BooleanTypeC

object NotC extends ExpressionInstance {

  object NotKey extends NodeClass

  object NotExpression extends NodeField

  override val key = NotKey

  override def getType(expression: Path, compilation: Compilation): Node = BooleanTypeC.booleanType

  override def toByteCode(expression: Path, compilation: Compilation): Seq[Node] = {
    ExpressionSkeleton.getToInstructions(compilation)(expression) ++ Seq(NotInstructionC.not)
  }

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val coreGrammar = grammars.find(ExpressionSkeleton.CoreGrammar)
    coreGrammar.addOption("!" ~> coreGrammar.as(NotExpression) asNode NotKey)
  }

  override def description: String = "Adds the ! (not) operator."
}
