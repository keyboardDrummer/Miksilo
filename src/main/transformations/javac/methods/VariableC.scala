package transformations.javac.methods

import core.transformation._
import transformations.bytecode.ByteCode
import transformations.javac.base.JavaMethodC
import transformations.javac.expressions.ExpressionC

object VariableC extends GrammarTransformation {


  override def dependencies: Set[ProgramTransformation] = Set(JavaMethodC)

  object VariableKey

  val variableNameKey = "name"

  def variable(name: String) = {
    new MetaObject(VariableKey) {
      data.put(variableNameKey, name)
    }
  }

  def getVariableName(variable: MetaObject) = variable(variableNameKey).asInstanceOf[String]

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    ExpressionC.getExpressionToLines(state).put(VariableKey, (variable: MetaObject) => {
      val methodCompiler = JavaMethodC.getMethodCompiler(state)
      val variableAddress = methodCompiler.variables.variables(getVariableName(variable)).offset
      Seq(ByteCode.integerLoad(variableAddress))
    })
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val expression = grammars.find(ExpressionC.ExpressionGrammar)
    val variableGrammar = identifier ^^ (name => variable(name.asInstanceOf[String]))
    expression.inner = expression.inner | variableGrammar
  }
}
