package transformations.javac.expressions.additive

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeLike}
import core.particles.path.Path
import transformations.bytecode.coreInstructions.integers.AddIntegersC
import transformations.bytecode.coreInstructions.longs.AddLongsC
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.bytecode.types.{IntTypeC, LongTypeC, TypeSkeleton}

object AdditionC extends ParticleWithGrammar with ExpressionInstance {

  val key = AdditionClazz

  override def toByteCode(addition: Path, state: CompilationState): Seq[Node] = {
    val toInstructions = ExpressionSkeleton.getToInstructions(state)
    val firstInstructions = toInstructions(getFirst(addition))
    val secondInstructions = toInstructions(getSecond(addition))
    firstInstructions ++ secondInstructions ++ (getType(addition, state) match {
      case x if x == IntTypeC.intType => Seq(AddIntegersC.addIntegers())
      case x if x == LongTypeC.longType => Seq(AddLongsC.addLongs())
      case _ => throw new NotImplementedError()
    })
  }

  override def getType(expression: Path, state: CompilationState): Node = {
    val getType = ExpressionSkeleton.getType(state)
    val firstType = getType(getFirst(expression))
    val secondType = getType(getSecond(expression))
    firstType match
    {
      case x if x == IntTypeC.intType =>
        TypeSkeleton.checkAssignableTo(state)(IntTypeC.intType, firstType)
        TypeSkeleton.checkAssignableTo(state)(IntTypeC.intType, secondType)
        IntTypeC.intType
      case x if x == LongTypeC.longType =>
        TypeSkeleton.checkAssignableTo(state)(LongTypeC.longType, firstType)
        TypeSkeleton.checkAssignableTo(state)(LongTypeC.longType, secondType)
        LongTypeC.longType
      case _ => throw new NotImplementedError()
    }

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
