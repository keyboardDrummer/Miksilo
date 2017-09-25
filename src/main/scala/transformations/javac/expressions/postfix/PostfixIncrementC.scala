package transformations.javac.expressions.postfix

import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import core.particles.path.Path
import core.particles.{CompilationState, Contract}
import transformations.bytecode.coreInstructions.integers.{IncrementIntegerDelta, LoadIntegerDelta}
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.javac.methods.MethodC
import transformations.bytecode.types.IntTypeC

object PostFixIncrementC extends ExpressionInstance {

  override val key: Key = PostfixIncrementKey

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton, MethodC, IncrementIntegerDelta)

  override def getType(expression: Path, state: CompilationState): Node = IntTypeC.intType

  override def toByteCode(plusPlus: Path, state: CompilationState): Seq[Node] = {
    val methodCompiler = MethodC.getMethodCompiler(state)
    val name: String = plusPlus(VariableKey).asInstanceOf[String]
    val variableAddress = methodCompiler.getVariables(plusPlus)(name).offset
    Seq(LoadIntegerDelta.load(variableAddress), IncrementIntegerDelta.integerIncrement(variableAddress, 1))
  }

  override def transformGrammars(grammars: GrammarCatalogue, state: CompilationState): Unit = {
    val coreGrammar = grammars.find(ExpressionSkeleton.CoreGrammar)
    val postFixIncrement = identifier <~ "++" asNode(PostfixIncrementKey, VariableKey)
    coreGrammar.addOption(postFixIncrement)
  }

  object PostfixIncrementKey extends Key

  object VariableKey extends Key

  override def description: String = "Adds the postfix ++ operator."
}
