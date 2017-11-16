package deltas.javac.expressions.prefix

import core.deltas.{Compilation, Language}
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeClass, NodeField}
import core.deltas.path.Path
import deltas.bytecode.extraBooleanInstructions.NotInstructionC
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import deltas.javac.types.BooleanTypeC

object NotC extends ExpressionInstance {

  object NotKey extends NodeClass

  object NotExpression extends NodeField

  override val key = NotKey

  override def getType(expression: Path, compilation: Compilation): Node = BooleanTypeC.booleanType

  override def toByteCode(expression: Path, compilation: Compilation): Seq[Node] = {
    ExpressionSkeleton.getToInstructions(compilation)(expression) ++ Seq(NotInstructionC.not)
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val coreGrammar = find(ExpressionSkeleton.CoreGrammar)
    coreGrammar.addOption("!" ~> coreGrammar.as(NotExpression) asNode NotKey)
  }

  override def description: String = "Adds the ! (not) operator."
}
