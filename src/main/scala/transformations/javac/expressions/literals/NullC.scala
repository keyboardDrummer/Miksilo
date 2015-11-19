package transformations.javac.expressions.literals

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import core.particles.path.Path
import transformations.bytecode.coreInstructions.objects.PushNullC
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}

object NullC extends ExpressionInstance {

  val _null = new Node(NullKey)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val parseNull = "null" ~> produce(_null)
    expressionGrammar.inner = expressionGrammar.inner | parseNull
  }

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton, PushNullC)

  object NullKey extends Key

  override val key: Key = NullKey

  override def getType(expression: Path, state: CompilationState): Node = ???

  override def toByteCode(expression: Path, state: CompilationState): Seq[Node] = {
    Seq(PushNullC.pushNull)
  }

  override def description: String = "Adds the usage of 'null'"
}
