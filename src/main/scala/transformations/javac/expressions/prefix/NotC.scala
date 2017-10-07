package transformations.javac.expressions.prefix

import core.particles.Language
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

  override def getType(expression: Path, state: Language): Node = BooleanTypeC.booleanType

  override def toByteCode(expression: Path, state: Language): Seq[Node] = {
    ExpressionSkeleton.getToInstructions(state)(expression(NotExpression).asInstanceOf[Path]) ++ Seq(NotInstructionC.not)
  }

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val coreGrammar = grammars.find(ExpressionSkeleton.CoreGrammar)
    coreGrammar.addOption("!" ~> coreGrammar.as(NotExpression) asNode NotKey)
  }

  override def description: String = "Adds the ! (not) operator."
}
