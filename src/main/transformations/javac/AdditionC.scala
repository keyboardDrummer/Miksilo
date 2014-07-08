package transformations.javac

import core.transformation.{GrammarTransformation, TransformationState, MetaObject, ProgramTransformation}
import transformations.javac.base.JavaBase
import transformations.bytecode.ByteCode
import scala.collection.mutable
import core.grammar.{seqr, Labelled, Grammar}

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

  override def dependencies: Set[ProgramTransformation] = Set(JavaBase)

  override def transformDelimiters(delimiters: mutable.HashSet[String]): Unit = delimiters += "+"

  override def transformGrammar(grammar: Grammar): Grammar = {
    val expressionGrammar = grammar.findGrammar(JavaBase.ExpressionGrammar).asInstanceOf[Labelled]
    lazy val pAddition : Grammar = (expressionGrammar <~ "+") ~ expressionGrammar ^^
      { case left seqr right => addition(left.asInstanceOf[MetaObject], right.asInstanceOf[MetaObject]) }
    expressionGrammar.inner = pAddition | expressionGrammar.inner
    grammar
  }
}
