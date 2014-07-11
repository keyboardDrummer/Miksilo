package transformations.javac.expressions

import core.grammar.{Grammar, seqr}
import core.transformation._
import transformations.bytecode.ByteCode
import transformations.javac.base.JavaBase

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
    JavaBase.getStatementToLines(state).put(clazz, (subtraction: MetaObject, compiler) => {
      val firstInstructions = JavaBase.statementToInstructions(getFirst(subtraction), compiler)
      val secondInstructions = JavaBase.statementToInstructions(getSecond(subtraction), compiler)
      firstInstructions ++ secondInstructions ++ Seq(ByteCode.subtractInteger)
    })
  }

  override def dependencies: Set[ProgramTransformation] = Set(AddAdditivePrecedence)

  override def transformDelimiters(delimiters: mutable.HashSet[String]): Unit = delimiters += "-"

  override def transformGrammars(grammars: GrammarCatalogue) {
    val additiveGrammar = grammars.find(AddAdditivePrecedence.AdditiveExpressionGrammar)
    val parseSubtraction: Grammar = (additiveGrammar <~ "-") ~ additiveGrammar ^^ { case left seqr right => subtraction(left, right)}
    additiveGrammar.inner = additiveGrammar.inner | parseSubtraction
  }

  def subtraction(first: Any, second: Any): MetaObject = subtraction(first.asInstanceOf[MetaObject], second.asInstanceOf[MetaObject])
}
