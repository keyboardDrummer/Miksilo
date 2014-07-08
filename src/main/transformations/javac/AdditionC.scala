package transformations.javac

import core.grammar.{Grammar, seqr}
import core.transformation.{GrammarTransformation, MetaObject, ProgramTransformation, TransformationState}
import transformations.bytecode.ByteCode
import transformations.javac.base.JavaBase

import scala.collection.mutable

object AdditionC extends GrammarTransformation {
  val clazz: String = "Addition"

  val firstKey: String = "first"

  val secondKey: String = "second"

  def addition(first: MetaObject, second: MetaObject) = new MetaObject(clazz) {
    data.put(firstKey, first)
    data.put(secondKey, second)
  }

  def getFirst(addition: MetaObject) = addition(firstKey).asInstanceOf[MetaObject]

  def getSecond(addition: MetaObject) = addition(secondKey).asInstanceOf[MetaObject]

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    JavaBase.getStatementToLines(state).put(clazz,(addition : MetaObject, compiler) => {
      val firstInstructions = JavaBase.statementToInstructions(getFirst(addition), compiler)
      val secondInstructions = JavaBase.statementToInstructions(getSecond(addition), compiler)
      firstInstructions ++ secondInstructions ++ Seq(ByteCode.addInteger)
    })
  }

  override def dependencies: Set[ProgramTransformation] = Set(AddAdditivePrecedence)

  override def transformDelimiters(delimiters: mutable.HashSet[String]): Unit = delimiters += "+"

  override def transformGrammar(grammar: Grammar): Grammar = {
    val additiveGrammar = grammar.findGrammar(AddAdditivePrecedence.AdditiveExpressionGrammar)
    val parseAddition : Grammar = (additiveGrammar <~ "+") ~ additiveGrammar ^^ { case left seqr right => addition(left, right) }
    additiveGrammar.inner = additiveGrammar.inner | parseAddition
    grammar
  }

  private def addition(first: Any, second: Any) : MetaObject = addition(first.asInstanceOf[MetaObject], second.asInstanceOf[MetaObject])
}
