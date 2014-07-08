package transformations.javac

import core.transformation.{GrammarTransformation, TransformationState, MetaObject, ProgramTransformation}
import transformations.javac.base.{JavaBaseParse, JavaBase}
import transformations.bytecode.ByteCode
import core.grammar.{seqr, Labelled, Grammar}
import scala.collection.mutable

object SubtractionC extends GrammarTransformation {
  val clazz: String = "Subtraction"

  val firstKey: String = "first"

  val secondKey: String = "second"

  def subtraction(first: MetaObject, second: MetaObject) = new MetaObject(clazz) {
    data.put(firstKey, first)
    data.put(secondKey, second)
  }

  def getFirst(subtraction: MetaObject) = subtraction(firstKey).asInstanceOf[MetaObject]

  def getSecond(subtraction: MetaObject) = subtraction(secondKey).asInstanceOf[MetaObject]

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    JavaBase.getStatementToLines(state).put(clazz,(subtraction : MetaObject, compiler) => {
      val firstInstructions = JavaBase.statementToInstructions(getFirst(subtraction), compiler)
      val secondInstructions = JavaBase.statementToInstructions(getSecond(subtraction), compiler)
      firstInstructions ++ secondInstructions ++ Seq(ByteCode.subtractInteger)
    })
  }

  override def dependencies: Set[ProgramTransformation] = Set(JavaBase)


  override def transformDelimiters(delimiters: mutable.HashSet[String]): Unit = delimiters += "-"

  override def transformGrammar(grammar: Grammar): Grammar = {
    val expressionGrammar = grammar.findGrammar(JavaBase.ExpressionGrammar).asInstanceOf[Labelled]
    lazy val pSubtraction : Grammar = (expressionGrammar <~ "-") ~ expressionGrammar ^^
      { case left seqr right => subtraction(left.asInstanceOf[MetaObject], right.asInstanceOf[MetaObject]) }
    expressionGrammar.inner = pSubtraction | expressionGrammar.inner
    grammar
  }
}
