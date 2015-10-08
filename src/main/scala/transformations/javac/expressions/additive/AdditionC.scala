package transformations.javac.expressions.additive

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeLike}
import core.particles.path.Path
import transformations.bytecode.coreInstructions.integers.AddIntegersC
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.bytecode.types.{IntTypeC, TypeSkeleton}

object AdditionC extends ParticleWithGrammar with ExpressionInstance {

  val key = AdditionClazz

  override def toByteCode(addition: Path, state: CompilationState): Seq[Node] = {
    val toInstructions = ExpressionSkeleton.getToInstructions(state)
    val firstInstructions = toInstructions(getFirst(addition))
    val secondInstructions = toInstructions(getSecond(addition))
    firstInstructions ++ secondInstructions ++ Seq(AddIntegersC.addInteger)
  }

  override def getType(expression: Path, state: CompilationState): Node = {
    val getType = ExpressionSkeleton.getType(state)
    val firstType = getType(getFirst(expression))
    val secondType = getType(getSecond(expression))
    TypeSkeleton.checkAssignableTo(state)(IntTypeC.intType, firstType)
    TypeSkeleton.checkAssignableTo(state)(IntTypeC.intType, secondType)
    IntTypeC.intType
  }

  def getFirst[T <: NodeLike](addition: T) = addition(FirstKey).asInstanceOf[T]

  def getSecond[T <: NodeLike](addition: T) = addition(SecondKey).asInstanceOf[T]

  override def dependencies: Set[Contract] = Set(AddAdditivePrecedence, AddIntegersC)

  override def transformGrammars(grammars: GrammarCatalogue) {
    val additiveGrammar = grammars.find(AddAdditivePrecedence.AdditiveExpressionGrammar)
    val parseAddition = (additiveGrammar <~~ "+") ~~ additiveGrammar ^^ parseMap(AdditionClazz, FirstKey, SecondKey)
    additiveGrammar.inner = additiveGrammar.inner | parseAddition
  }

  def addition(first: Node, second: Node) = new Node(AdditionClazz, FirstKey -> first, SecondKey -> second)

  object AdditionClazz

  object FirstKey

  object SecondKey

  override def description: String = "Adds the + operator."
}
