package transformations.javac.expressions.literals

import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import transformations.bytecode.coreInstructions.objects.PushNullC
import transformations.javac.expressions.{ExpressionC, ExpressionInstance}

object NullC extends ExpressionInstance {

  val _null = new MetaObject(NullKey)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    val parseNull = "null" ~> produce(_null)
    expressionGrammar.inner = expressionGrammar.inner | parseNull
  }

  override def dependencies: Set[Contract] = Set(ExpressionC, PushNullC)

  object NullKey

  override val key: AnyRef = NullKey

  override def getType(expression: MetaObject, state: TransformationState): MetaObject = ???

  override def toByteCode(expression: MetaObject, state: TransformationState): Seq[MetaObject] = {
    Seq(PushNullC.pushNull)
  }
}
