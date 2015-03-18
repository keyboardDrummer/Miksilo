package transformations.javac.expressions.additive

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, MetaLike}
import core.particles.path.Path
import transformations.bytecode.coreInstructions.integers.SubtractIntegerC
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}

object SubtractionC extends ExpressionInstance {
  object SubtractionKey
  object FirstKey
  object SecondKey

  def getFirst[T <: MetaLike](subtraction: T) = subtraction(FirstKey).asInstanceOf[T]

  def getSecond[T <: MetaLike](subtraction: T) = subtraction(SecondKey).asInstanceOf[T]

  override def dependencies: Set[Contract] = Set(AddAdditivePrecedence, SubtractIntegerC)

  override def transformGrammars(grammars: GrammarCatalogue) {
    val additiveGrammar = grammars.find(AddAdditivePrecedence.AdditiveExpressionGrammar)
    val parseSubtraction = (additiveGrammar <~~ "-") ~~  additiveGrammar ^^ parseMap(SubtractionKey, FirstKey, SecondKey)
    additiveGrammar.addOption(parseSubtraction)
  }

  def subtraction(first: Any, second: Any): Node = subtraction(first.asInstanceOf[Node], second.asInstanceOf[Node])

  def subtraction(first: Node, second: Node) = new Node(SubtractionKey,
    FirstKey -> first,
    SecondKey -> second)

  override val key: AnyRef = SubtractionKey

  override def getType(expression: Path, state: CompilationState): Node = ???

  override def toByteCode(subtraction: Path, state: CompilationState): Seq[Node] = {
    val toInstructions = ExpressionSkeleton.getToInstructions(state)
    val firstInstructions = toInstructions(getFirst(subtraction))
    val secondInstructions = toInstructions(getSecond(subtraction))
    firstInstructions ++ secondInstructions ++ Seq(SubtractIntegerC.subtractInteger)
  }

  override def description: String = "Adds the - operator."
}
