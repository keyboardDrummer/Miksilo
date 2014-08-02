package transformations.javac.expressions

import core.grammar._
import core.transformation._

import scala.collection.mutable

object EqualityC extends GrammarTransformation {
  override def dependencies: Set[Contract] = Set(AddEqualityPrecedence)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    ExpressionC.getGetTypeRegistry(state).put(EqualityC, equality => {
      val first = getFirst(equality)
      val second = getSecond(equality)
      val toInstructions = ExpressionC.getToInstructions(state)
      toInstructions(first) ++ toInstructions(second) ++ ???
    })
  }

  def getFirst(equality: MetaObject) = equality(FirstKey).asInstanceOf[MetaObject]

  def getSecond(equality: MetaObject) = equality(SecondKey).asInstanceOf[MetaObject]

  override def transformDelimiters(delimiters: mutable.HashSet[String]): Unit = delimiters += "=="

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    val equalityGrammar = grammars.find(AddEqualityPrecedence.EqualityExpressionGrammar)
    val parseEquality = (expressionGrammar <~ "==") ~ expressionGrammar ^^ { case left seqr right => equality(left.asInstanceOf[MetaObject], right.asInstanceOf[MetaObject])}
    equalityGrammar.inner = equalityGrammar.inner | equalityGrammar
  }

  def equality(first: MetaObject, second: MetaObject) = new MetaObject(EqualityKey, FirstKey -> first, SecondKey -> second)

  object EqualityKey

  object FirstKey

  object SecondKey

}
