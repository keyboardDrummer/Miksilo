package transformations.javac.expressions.prefix

import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import core.particles.path.Path
import core.particles.CompilationState
import transformations.bytecode.extraBooleanInstructions.NotInstructionC
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.javac.types.BooleanTypeC

object NotC extends ExpressionInstance {

  object NotKey extends Key

  object NotExpression extends Key

  override val key: Key = NotKey

  override def getType(expression: Path, state: CompilationState): Node = BooleanTypeC.booleanType

  override def toByteCode(expression: Path, state: CompilationState): Seq[Node] = {
    ExpressionSkeleton.getToInstructions(state)(expression(NotExpression).asInstanceOf[Path]) ++ Seq(NotInstructionC.not)
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val coreGrammar = grammars.find(ExpressionSkeleton.CoreGrammar)
    coreGrammar.addOption("!" ~> coreGrammar asNode(NotKey, NotExpression))
  }

  override def description: String = "Adds the ! (not) operator."
}
