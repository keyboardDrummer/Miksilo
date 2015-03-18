package transformations.javac.expressions.additive

import core.particles._
import core.particles.grammars.GrammarCatalogue
import transformations.bytecode.coreInstructions.integers.AddIntegersC
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.types.{IntTypeC, TypeSkeleton}

object AdditionC extends ParticleWithGrammar with ExpressionInstance {

  val key = AdditionClazz

  override def toByteCode(addition: Path, state: CompilationState): Seq[MetaObject] = {
    val toInstructions = ExpressionSkeleton.getToInstructions(state)
    val firstInstructions = toInstructions(getFirst(addition))
    val secondInstructions = toInstructions(getSecond(addition))
    firstInstructions ++ secondInstructions ++ Seq(AddIntegersC.addInteger)
  }

  override def getType(expression: Path, state: CompilationState): MetaObject = {
    val getType = ExpressionSkeleton.getType(state)
    val firstType = getType(getFirst(expression))
    val secondType = getType(getSecond(expression))
    TypeSkeleton.checkAssignableTo(state)(IntTypeC.intType, firstType)
    TypeSkeleton.checkAssignableTo(state)(IntTypeC.intType, secondType)
    IntTypeC.intType
  }

  def getFirst[T <: MetaLike](addition: T) = addition(FirstKey).asInstanceOf[T]

  def getSecond[T <: MetaLike](addition: T) = addition(SecondKey).asInstanceOf[T]

  override def dependencies: Set[Contract] = Set(AddAdditivePrecedence, AddIntegersC)

  override def transformGrammars(grammars: GrammarCatalogue) {
    val additiveGrammar = grammars.find(AddAdditivePrecedence.AdditiveExpressionGrammar)
    val parseAddition = (additiveGrammar <~~ "+") ~~ additiveGrammar ^^ parseMap(AdditionClazz, FirstKey, SecondKey)
    additiveGrammar.inner = additiveGrammar.inner | parseAddition
  }

  private def addition(first: Any, second: Any): MetaObject = addition(first.asInstanceOf[MetaObject], second.asInstanceOf[MetaObject])

  def addition(first: MetaObject, second: MetaObject) = new MetaObject(AdditionClazz, FirstKey -> first, SecondKey -> second)

  object AdditionClazz

  object FirstKey

  object SecondKey

  override def description: String = "Adds the + operator."
}
