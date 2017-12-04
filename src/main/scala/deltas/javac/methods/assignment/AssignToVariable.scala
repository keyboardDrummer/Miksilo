package deltas.javac.methods.assignment

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.Node
import core.deltas.path.Path
import deltas.bytecode.coreInstructions.integers.StoreIntegerDelta
import deltas.bytecode.coreInstructions.longs.StoreLongDelta
import deltas.bytecode.coreInstructions.objects.StoreAddressDelta
import deltas.bytecode.types.ArrayTypeC.ArrayTypeKey
import deltas.bytecode.types.IntTypeC.IntTypeKey
import deltas.bytecode.types.LongTypeC.LongTypeKey
import deltas.bytecode.types.{ObjectTypeDelta, TypeSkeleton}
import deltas.javac.methods.{MethodDelta, VariableDelta, VariableInfo}

object AssignToVariable extends DeltaWithGrammar {

  override def dependencies: Set[Contract] = Set(AssignmentSkeleton, VariableDelta)

  override def inject(state: Language): Unit = {
    AssignmentSkeleton.getRegistry(state).assignFromStackByteCodeRegistry.put(VariableDelta.VariableKey,
      (compilation, targetVariable: Path) => {
      val methodCompiler = MethodDelta.getMethodCompiler(compilation)
      val target = VariableDelta.getVariableName(targetVariable)
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

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val targetGrammar = grammars.find(AssignmentSkeleton.AssignmentTargetGrammar)
    val variableGrammar = grammars.find(VariableDelta.VariableGrammar)
    targetGrammar.addOption(variableGrammar)
  }

  override def description: String = "Enables assigning to a variable."
}
