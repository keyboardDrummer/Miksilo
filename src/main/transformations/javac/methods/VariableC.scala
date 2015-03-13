package transformations.javac.methods

import core.particles._
import core.particles.grammars.GrammarCatalogue
import transformations.bytecode.coreInstructions.integers.LoadIntegerC
import transformations.bytecode.coreInstructions.longs.LoadLongC
import transformations.bytecode.coreInstructions.objects.LoadAddressC
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.types.{BooleanTypeC, IntTypeC, LongTypeC, ObjectTypeC}

object VariableC extends ExpressionInstance {

  override def dependencies: Set[Contract] = Set(MethodC, LoadIntegerC)

  def getVariableName(variable: MetaObject) = variable(VariableNameKey).asInstanceOf[String]

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val core = grammars.find(ExpressionSkeleton.CoreGrammar)
    val variableGrammar = grammars.create(VariableGrammar, identifier ^^ parseMap(VariableKey, VariableNameKey))
    core.addOption(variableGrammar)
  }

  object VariableGrammar

  def variable(name: String) = new MetaObject(VariableKey, VariableNameKey -> name)

  object VariableNameKey

  object VariableKey

  override val key: AnyRef = VariableKey

  override def getType(variable: MetaObject, state: CompilationState): MetaObject = {
    val methodCompiler = MethodC.getMethodCompiler(state)
    methodCompiler.variables(VariableC.getVariableName(variable))._type
  }

  override def toByteCode(variable: MetaObject, state: CompilationState): Seq[MetaObject] = {
    val methodCompiler = MethodC.getMethodCompiler(state)
    val name: String = getVariableName(variable)
    val variableInfo: VariableInfo = methodCompiler.variables(name)
    val variableAddress = variableInfo.offset
    val _type = variableInfo._type
    Seq(_type.clazz match {
      case BooleanTypeC.BooleanTypeKey => LoadIntegerC.load(variableAddress)
      case IntTypeC.IntTypeKey => LoadIntegerC.load(variableAddress)
      case LongTypeC.LongTypeKey => LoadLongC.load(variableAddress)
      case ObjectTypeC.ObjectTypeKey => LoadAddressC.addressLoad(variableAddress)
    })
  }

  override def description: String = "Enables referencing a variable."
}
