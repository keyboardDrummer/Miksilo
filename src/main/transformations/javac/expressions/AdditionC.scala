package transformations.javac.expressions

import core.grammar.{Grammar, seqr}
import core.transformation._
import transformations.bytecode.instructions.AddIntegersC

import scala.collection.mutable

object AdditionC extends GrammarTransformation {

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    ExpressionC.getExpressionToLines(state).put(Clazz, (addition: MetaObject) => {
      val toInstructions = ExpressionC.getToInstructions(state)
      val firstInstructions = toInstructions(getFirst(addition))
      val secondInstructions = toInstructions(getSecond(addition))
      firstInstructions ++ secondInstructions ++ Seq(AddIntegersC.addInteger)
    })
  }

  def getFirst(addition: MetaObject) = addition(FirstKey).asInstanceOf[MetaObject]

  def getSecond(addition: MetaObject) = addition(SecondKey).asInstanceOf[MetaObject]

  override def dependencies: Set[Contract] = Set(AddAdditivePrecedence, AddIntegersC)

  override def transformDelimiters(delimiters: mutable.HashSet[String]): Unit = delimiters += "+"

  override def transformGrammars(grammars: GrammarCatalogue) {
    val additiveGrammar = grammars.find(AddAdditivePrecedence.AdditiveExpressionGrammar)
    val parseAddition: Grammar = (additiveGrammar <~ "+") ~ additiveGrammar ^^ { case left seqr right => addition(left, right)}
    additiveGrammar.inner = additiveGrammar.inner | parseAddition
  }

  private def addition(first: Any, second: Any): MetaObject = addition(first.asInstanceOf[MetaObject], second.asInstanceOf[MetaObject])

  def addition(first: MetaObject, second: MetaObject) = new MetaObject(Clazz) {
    data.put(FirstKey, first)
    data.put(SecondKey, second)
  }

  object Clazz

  object FirstKey

  object SecondKey

}
