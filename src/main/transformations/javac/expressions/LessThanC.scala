package transformations.javac.expressions

import core.grammar.{Grammar, seqr}
import core.transformation._
import transformations.bytecode.instructions.IntegerConstantC
import transformations.bytecode.{ByteCodeSkeleton, InferredStackFrames, LabelledJumps}

import scala.collection.mutable

object LessThanC extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(AddRelationalPrecedence, IntegerConstantC)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    ExpressionC.getExpressionToLines(state).put(LessThanKey, lessThan => {
      val toInstructions = ExpressionC.getToInstructions(state)
      val firstInstructions = toInstructions(getFirst(lessThan))
      val secondInstructions = toInstructions(getSecond(lessThan))
      val falseStartLabel = state.getUniqueLabel("falseStart")
      val endLabel = state.getUniqueLabel("end")
      firstInstructions ++ secondInstructions ++
        Seq(LabelledJumps.ifIntegerCompareGreater(falseStartLabel),
          IntegerConstantC.integerConstant(1),
          LabelledJumps.goTo(endLabel),
          InferredStackFrames.label(falseStartLabel),
          IntegerConstantC.integerConstant(0),
          InferredStackFrames.label(endLabel))
    })
  }

  def getFirst(lessThan: MetaObject) = ByteCodeSkeleton.getInstructionArguments(lessThan)(0).asInstanceOf[MetaObject]

  def getSecond(lessThan: MetaObject) = ByteCodeSkeleton.getInstructionArguments(lessThan)(1).asInstanceOf[MetaObject]

  override def transformDelimiters(delimiters: mutable.HashSet[String]): Unit = delimiters += "<"

  override def transformGrammars(grammars: GrammarCatalogue) {
    val relationalGrammar = grammars.find(AddRelationalPrecedence.RelationalExpressionGrammar)
    val parseLessThan: Grammar = (relationalGrammar <~ "<") ~ relationalGrammar ^^ { case left seqr right => lessThan(left, right)}
    relationalGrammar.inner = relationalGrammar.inner | parseLessThan
  }

  private def lessThan(left: Any, right: Any): MetaObject = lessThan(left.asInstanceOf[MetaObject], right.asInstanceOf[MetaObject])

  def lessThan(first: MetaObject, second: MetaObject) = ByteCodeSkeleton.instruction(LessThanKey, Seq(first, second))

  object LessThanKey

  object LessThanFirst

  object LessThanSecond

}
