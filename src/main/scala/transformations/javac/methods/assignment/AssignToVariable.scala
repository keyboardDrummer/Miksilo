package transformations.javac.methods.assignment

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import core.particles.path.Path
import transformations.bytecode.coreInstructions.integers.StoreIntegerDelta
import transformations.bytecode.coreInstructions.longs.StoreLongDelta
import transformations.bytecode.coreInstructions.objects.StoreAddressDelta
import transformations.bytecode.types.ArrayTypeC.ArrayTypeKey
import transformations.bytecode.types.IntTypeC.IntTypeKey
import transformations.bytecode.types.LongTypeC.LongTypeKey
import transformations.bytecode.types.{ObjectTypeDelta, TypeSkeleton}
import transformations.javac.methods.{MethodDelta, VariableC, VariableInfo}

object AssignToVariable extends DeltaWithGrammar {

  override def dependencies: Set[Contract] = Set(AssignmentSkeleton, VariableC)

  override def inject(state: Language): Unit = {
    AssignmentSkeleton.getRegistry(state).assignFromStackByteCodeRegistry.put(VariableC.VariableKey,
      (compilation, targetVariable: Path) => {
      val methodCompiler = MethodDelta.getMethodCompiler(compilation)
      val target = VariableC.getVariableName(targetVariable)
      val variableInfo = methodCompiler.getVariables(targetVariable)(target)
      val byteCodeType = TypeSkeleton.toStackType(variableInfo._type, state)
      Seq(getStoreInstruction(variableInfo, byteCodeType))
    })
    super.inject(state)
  }

  def getStoreInstruction(variableInfo: VariableInfo, byteCodeType: Node): Node = {
    byteCodeType.clazz match {
      case IntTypeKey => StoreIntegerDelta.integerStore(variableInfo.offset)
      case ObjectTypeDelta.ObjectStackType => StoreAddressDelta.addressStore(variableInfo.offset)
      case ArrayTypeKey => StoreAddressDelta.addressStore(variableInfo.offset)
      case LongTypeKey => StoreLongDelta.longStore(variableInfo.offset)
    }
  }

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val targetGrammar = grammars.find(AssignmentSkeleton.AssignmentTargetGrammar)
    val variableGrammar = grammars.find(VariableC.VariableGrammar)
    targetGrammar.addOption(variableGrammar)
  }

  override def description: String = "Enables assigning to a variable."
}
