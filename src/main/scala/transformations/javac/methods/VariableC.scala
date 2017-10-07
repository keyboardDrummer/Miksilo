package transformations.javac.methods

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField}
import core.particles.path.Path
import transformations.bytecode.coreInstructions.integers.LoadIntegerDelta
import transformations.bytecode.coreInstructions.longs.LoadLongDelta
import transformations.bytecode.coreInstructions.objects.LoadAddressDelta
import transformations.bytecode.types.{IntTypeC, LongTypeC, ObjectTypeDelta}
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.javac.types.BooleanTypeC

object VariableC extends ExpressionInstance {

  override def dependencies: Set[Contract] = Set(MethodC, LoadIntegerDelta)

  def getVariableName(variable: Node) = variable(VariableNameKey).asInstanceOf[String]

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val core = grammars.find(ExpressionSkeleton.CoreGrammar)
    val variableGrammar = grammars.create(VariableGrammar, identifier.as(VariableNameKey) asNode VariableKey)
    core.addOption(variableGrammar)
  }

  object VariableGrammar

  def variable(name: String) = new Node(VariableKey, VariableNameKey -> name)

  object VariableNameKey extends NodeField

  object VariableKey extends NodeClass

  override val key = VariableKey

  override def getType(variable: Path, state: Language): Node = {
    getVariableInfo(variable, state)._type
  }

  def getVariableInfo(variable: Path, state: Language): VariableInfo = {
    MethodC.getMethodCompiler(state).getVariables(variable)(VariableC.getVariableName(variable))
  }

  override def toByteCode(variable: Path, state: Language): Seq[Node] = {
    val variableInfo: VariableInfo = getVariableInfo(variable, state)
    val variableAddress = variableInfo.offset
    val _type = variableInfo._type
    Seq(_type.clazz match {
      case BooleanTypeC.BooleanTypeKey => LoadIntegerDelta.load(variableAddress)
      case IntTypeC.IntTypeKey => LoadIntegerDelta.load(variableAddress)
      case LongTypeC.LongTypeKey => LoadLongDelta.load(variableAddress)
      case ObjectTypeDelta.ObjectTypeKey => LoadAddressDelta.addressLoad(variableAddress)
    })
  }

  override def description: String = "Enables referencing a variable."
}
