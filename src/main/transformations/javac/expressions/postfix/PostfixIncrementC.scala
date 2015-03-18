package transformations.javac.expressions.postfix

import core.particles.grammars.GrammarCatalogue
import core.particles.path.Path
import core.particles.{CompilationState, Contract, MetaObject}
import transformations.bytecode.coreInstructions.integers.{IncrementIntegerC, LoadIntegerC}
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.javac.methods.MethodC
import transformations.types.IntTypeC

object PostFixIncrementC extends ExpressionInstance {

  override val key: AnyRef = PostfixIncrementKey

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton, MethodC, IncrementIntegerC)

  override def getType(expression: Path, state: CompilationState): MetaObject = IntTypeC.intType

  override def toByteCode(plusPlus: Path, state: CompilationState): Seq[MetaObject] = {
    val methodCompiler = MethodC.getMethodCompiler(state)
    val name: String = plusPlus(VariableKey).asInstanceOf[String]
    val variableAddress = getVariables(state, plusPlus)(name).offset
    Seq(LoadIntegerC.load(variableAddress), IncrementIntegerC.integerIncrement(variableAddress, 1))
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val coreGrammar = grammars.find(ExpressionSkeleton.CoreGrammar)
    val postFixIncrement = identifier <~ "++" ^^ parseMap(PostfixIncrementKey, VariableKey)
    coreGrammar.addOption(postFixIncrement)
  }

  object PostfixIncrementKey

  object VariableKey

  override def description: String = "Adds the postfix ++ operator."
}
