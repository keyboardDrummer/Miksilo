package deltas.javac.methods

import core.particles._
import core.particles.grammars.LanguageGrammars
import core.particles.node._
import core.particles.path.Path
import deltas.bytecode.coreInstructions.integers.LoadIntegerDelta
import deltas.bytecode.coreInstructions.longs.LoadLongDelta
import deltas.bytecode.coreInstructions.objects.LoadAddressDelta
import deltas.bytecode.types.{IntTypeC, LongTypeC, ObjectTypeDelta}
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import deltas.javac.types.BooleanTypeC

object VariableC extends ExpressionInstance {

  override def dependencies: Set[Contract] = Set(MethodDelta, LoadIntegerDelta)

  def getVariableName(variable: Node) = variable(VariableNameKey).asInstanceOf[String]

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val core = find(ExpressionSkeleton.CoreGrammar)
    val variableGrammar = create(VariableGrammar, identifier.as(VariableNameKey) asNode VariableKey)
    core.addOption(variableGrammar)
  }

  object VariableGrammar extends GrammarKey

  def variable(name: String) = new Node(VariableKey, VariableNameKey -> name)

  object VariableNameKey extends NodeField

  object VariableKey extends NodeClass

  override val key = VariableKey

  override def getType(variable: Path, compilation: Compilation): Node = {
    getVariableInfo(variable, compilation)._type
  }

  def getVariableInfo(variable: Path, compilation: Compilation): VariableInfo = {
    MethodDelta.getMethodCompiler(compilation).getVariables(variable)(VariableC.getVariableName(variable))
  }

  override def toByteCode(variable: Path, compilation: Compilation): Seq[Node] = {
    val variableInfo: VariableInfo = getVariableInfo(variable, compilation)
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
