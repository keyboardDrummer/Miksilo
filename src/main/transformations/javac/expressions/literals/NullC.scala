package transformations.javac.expressions.literals

import core.particles._
import core.particles.grammars.GrammarCatalogue
import transformations.bytecode.coreInstructions.objects.PushNullC
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}

object NullC extends ExpressionInstance {

  val _null = new MetaObject(NullKey)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val parseNull = "null" ~> produce(_null)
    expressionGrammar.inner = expressionGrammar.inner | parseNull
  }

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton, PushNullC)

  object NullKey

  override val key: AnyRef = NullKey

  override def getType(expression: MetaObject, state: CompilationState): MetaObject = ???

  override def toByteCode(expression: MetaObject, state: CompilationState): Seq[MetaObject] = {
    Seq(PushNullC.pushNull)
  }

  override def description: String = "Adds the usage of 'null'"
}
