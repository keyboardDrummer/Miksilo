package transformations.javac.expressions.literals

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import core.particles.path.Path
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.constants.IntegerInfoConstant
import transformations.bytecode.coreInstructions.integers.{LoadConstantIntC, SmallIntegerConstantC}
import transformations.bytecode.types.IntTypeC
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}

object IntLiteralC extends ExpressionInstance {
  val key = IntLiteralKey

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton, SmallIntegerConstantC)

  override def transformGrammars(grammars: GrammarCatalogue, state: CompilationState): Unit = {
    val inner = number ^^(number => Integer.parseInt(number.asInstanceOf[String]), i => Some(i))
    val parseNumber = inner.asNode(IntLiteralKey, ValueKey)
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    expressionGrammar.inner = expressionGrammar.inner | parseNumber
  }

  def literal(value: Int) = new Node(IntLiteralKey, ValueKey -> value)

  override def toByteCode(literal: Path, state: CompilationState): Seq[Node] = {
    val value: Int = getValue(literal)
    if (-1 <= value && value <= 5) {
      val node = literal.current.shallowClone
      node.replaceWith(SmallIntegerConstantC.integerConstant(value), keepData = true)
      Seq(node) //TODO dit mooier maken. Maak de nieuwe node gewoon en en schuif deze over de oude node.
    }
    else
    {
      Seq(LoadConstantIntC.integerConstant(IntegerInfoConstant.construct(value)))
    }
  }

  def getValue(literal: Node): Int = literal(ValueKey).asInstanceOf[Int]

  override def getType(expression: Path, state: CompilationState): Node = IntTypeC.intType

  object IntLiteralKey extends Key

  object ValueKey extends Key

  override def description: String = "Adds the usage of int literals."
}
