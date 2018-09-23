package deltas.javac.expressions.relational

import core.deltas.Contract
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node._
import core.language.{Compilation, Language}
import deltas.bytecode.coreInstructions.integers.SmallIntegerConstantDelta
import deltas.bytecode.extraBooleanInstructions.{GreaterThanInstructionDelta, LessThanInstructionDelta}
import deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}

object GreaterThanDelta extends ComparisonOperatorDelta with ConvertsToByteCodeDelta { //TODO move more code into comparisonOperatorDelta.

  override def description: String = "Adds the > operator."

  override def dependencies: Set[Contract] = Set(AddRelationalPrecedence, SmallIntegerConstantDelta, LessThanInstructionDelta)

  override def toByteCode(expression: NodePath, compilation: Compilation): Seq[Node] = {
    val toInstructions = ToByteCodeSkeleton.getToInstructions(compilation)
    val greaterThan: ComparisonOperator[NodePath] = expression
    val firstInstructions = toInstructions(greaterThan.left)
    val secondInstructions = toInstructions(greaterThan.right)
    firstInstructions ++ secondInstructions ++ Seq(GreaterThanInstructionDelta.greaterThanInstruction)
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    import grammars._
    val relationalGrammar = find(AddRelationalPrecedence.RelationalExpressionGrammar)
    val parseLessThan = ((relationalGrammar.as(Left) ~~< ">") ~~ relationalGrammar.as(Right)).asNode(Shape)
    relationalGrammar.addAlternative(parseLessThan)
  }
}
