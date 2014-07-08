package transformations.javac

import core.transformation.{GrammarTransformation, TransformationState, MetaObject, ProgramTransformation}
import transformations.javac.base.{JavaBaseParse, JavaBase}
import transformations.bytecode.{InferredStackFrames, LabelledJumps, ByteCode}
import core.grammar.{Labelled, Grammar, seqr}
import scala.collection.mutable

object LessThanC extends GrammarTransformation {
  object LessThanKey
  object LessThanFirst
  object LessThanSecond
  def lessThan(first: MetaObject, second: MetaObject) = ByteCode.instruction(LessThanKey, Seq(first,second))
  def getFirst(lessThan: MetaObject) = ByteCode.getInstructionArguments(lessThan)(0).asInstanceOf[MetaObject]
  def getSecond(lessThan: MetaObject) = ByteCode.getInstructionArguments(lessThan)(1).asInstanceOf[MetaObject]

  override def dependencies: Set[ProgramTransformation] = Set(JavaBase)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    JavaBase.getStatementToLines(state).put(LessThanKey,(subtraction : MetaObject, compiler) => {
      val firstInstructions = JavaBase.statementToInstructions(getFirst(subtraction), compiler)
      val secondInstructions = JavaBase.statementToInstructions(getSecond(subtraction), compiler)
      val falseStartLabel = state.getUniqueLabel("falseStart")
      val endLabel = state.getUniqueLabel("end")
      firstInstructions ++ secondInstructions ++
        Seq(LabelledJumps.ifIntegerCompareGreater(falseStartLabel),
          ByteCode.integerConstant(1),
          LabelledJumps.goTo(endLabel),
          InferredStackFrames.label(falseStartLabel),
          ByteCode.integerConstant(0),
          InferredStackFrames.label(endLabel))
    })
  }


  override def transformDelimiters(delimiters: mutable.HashSet[String]): Unit = delimiters += "<"

  override def transformGrammar(grammar: Grammar): Grammar = {
    val expressionGrammar = grammar.findGrammar(JavaBase.ExpressionGrammar).asInstanceOf[Labelled]
    lazy val pLessThan : Grammar = (expressionGrammar <~ "<") ~ expressionGrammar ^^
      { case left seqr right => lessThan(left.asInstanceOf[MetaObject], right.asInstanceOf[MetaObject]) }
    expressionGrammar.inner = pLessThan | expressionGrammar.inner
    grammar
  }
}
