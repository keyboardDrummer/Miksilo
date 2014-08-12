package transformations.javac.expressions.postfix

import core.transformation.grammars.GrammarCatalogue
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.coreInstructions.{IncrementIntegerC, LoadIntegerC}
import transformations.javac.expressions.ExpressionInstance
import transformations.javac.methods.{MethodC, VariableC}
import transformations.types.IntTypeC

object PostfixPlusPlusC extends ExpressionInstance {

  override val key: AnyRef = PostfixPlusPlusKey

  override def dependencies: Set[Contract] = Set(PostFixPrecedenceC, VariableC, IncrementIntegerC)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val postFixGrammar = grammars.find(PostFixPrecedenceC.PostFixGrammar)
    val plusPlus = identifier <~ "++" ^^
      (name => new MetaObject(PostfixPlusPlusKey, VariableKey -> name))
    postFixGrammar.inner = postFixGrammar.inner | plusPlus
  }

  override def getType(expression: MetaObject, state: TransformationState): MetaObject = IntTypeC.intType

  override def toByteCode(plusPlus: MetaObject, state: TransformationState): Seq[MetaObject] = {
    val methodCompiler = MethodC.getMethodCompiler(state)
    val name: String = plusPlus(VariableKey).asInstanceOf[String]
    val variableAddress = methodCompiler.variables(name).offset
    Seq(LoadIntegerC.integerLoad(variableAddress), IncrementIntegerC.integerIncrement(variableAddress, 1))
  }

  object PostfixPlusPlusKey

  object VariableKey

}
