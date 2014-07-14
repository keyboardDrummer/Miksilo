package transformations.javac.expressions

import core.grammar.{Grammar, seqr}
import core.transformation._
import transformations.bytecode.{ByteCode, InferredStackFrames, LabelledJumps}

import scala.collection.mutable

object LessThanC extends GrammarTransformation {

  object LessThanKey

  object LessThanFirst

  object LessThanSecond

  def lessThan(first: MetaObject, second: MetaObject) = ByteCode.instruction(LessThanKey, Seq(first, second))

  def getFirst(lessThan: MetaObject) = ByteCode.getInstructionArguments(lessThan)(0).asInstanceOf[MetaObject]

  def getSecond(lessThan: MetaObject) = ByteCode.getInstructionArguments(lessThan)(1).asInstanceOf[MetaObject]

  override def dependencies: Set[Contract] = Set(AddRelationalPrecedence)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    ExpressionC.getExpressionToLines(state).put(LessThanKey, subtraction => {
      val toInstructions = ExpressionC.getToInstructions(state)
      val firstInstructions = toInstructions(getFirst(subtraction))
      val secondInstructions = toInstructions(getSecond(subtraction))
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

  override def transformGrammars(grammars: GrammarCatalogue) {
    val relationalGrammar = grammars.find(AddRelationalPrecedence.RelationalExpressionGrammar)
    val parseLessThan: Grammar = (relationalGrammar <~ "<") ~ relationalGrammar ^^ { case left seqr right => lessThan(left, right)}
    relationalGrammar.inner = relationalGrammar.inner | parseLessThan
  }

  private def lessThan(left: Any, right: Any): MetaObject = lessThan(left.asInstanceOf[MetaObject], right.asInstanceOf[MetaObject])
}
