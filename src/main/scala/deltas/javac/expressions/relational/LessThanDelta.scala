package deltas.javac.expressions.relational

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node._
import core.language.{Compilation, Language}
import deltas.bytecode.coreInstructions.integers.SmallIntegerConstantDelta
import deltas.bytecode.extraBooleanInstructions.LessThanInstructionDelta
import deltas.javac.expressions.ExpressionSkeleton

object LessThanDelta extends ComparisonOperatorDelta { //TODO move more code into comparisonOperatorDelta.

  override def description: String = "Adds the < operator."

  override def dependencies: Set[Contract] = Set(AddRelationalPrecedence, SmallIntegerConstantDelta, LessThanInstructionDelta)

  override def toByteCode(lessThan: NodePath, compilation: Compilation): Seq[Node] = {
    val toInstructions = ExpressionSkeleton.getToInstructions(compilation)
    val firstInstructions = toInstructions(lessThan.left)
    val secondInstructions = toInstructions(lessThan.right)
    firstInstructions ++ secondInstructions ++ Seq(LessThanInstructionDelta.lessThanInstruction)
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    import grammars._
    val relationalGrammar = find(AddRelationalPrecedence.RelationalExpressionGrammar)
    val parseLessThan = ((relationalGrammar.as(Left) ~~< "<") ~~ relationalGrammar.as(Right)).asNode(Shape)
    relationalGrammar.addAlternative(parseLessThan)
  }
}
