package transformations.javac.methods.assignment

import core.particles.grammars.GrammarCatalogue
import core.particles._
import core.particles.node.Node
import core.particles.path.Path
import transformations.bytecode.coreInstructions.integers.StoreIntegerC
import transformations.bytecode.coreInstructions.longs.StoreLongC
import transformations.bytecode.coreInstructions.objects.StoreAddressC
import transformations.javac.methods.{MethodC, VariableC, VariableInfo}
import transformations.bytecode.types.ArrayTypeC.ArrayTypeKey
import transformations.bytecode.types.IntTypeC.IntTypeKey
import transformations.bytecode.types.LongTypeC.LongTypeKey
import transformations.bytecode.types.ObjectTypeC.ObjectTypeKey
import transformations.bytecode.types.TypeSkeleton

object AssignToVariable extends DeltaWithGrammar {

  override def dependencies: Set[Contract] = Set(AssignmentSkeleton, VariableC)

  override def inject(state: CompilationState): Unit = {
    AssignmentSkeleton.getState(state).assignFromStackByteCodeRegistry.put(VariableC.VariableKey, (targetVariable: Path) => {
      val methodCompiler = MethodC.getMethodCompiler(state)
      val target = VariableC.getVariableName(targetVariable)
      val variableInfo = methodCompiler.getVariables(targetVariable)(target)
      val byteCodeType = TypeSkeleton.toStackType(variableInfo._type, state)
      Seq(getStoreInstruction(variableInfo, byteCodeType))
    })
    super.inject(state)
  }

  def getStoreInstruction(variableInfo: VariableInfo, byteCodeType: Node): Node = {
    byteCodeType.clazz match {
      case IntTypeKey => StoreIntegerC.integerStore(variableInfo.offset)
      case ObjectTypeKey => StoreAddressC.addressStore(variableInfo.offset)
      case ArrayTypeKey => StoreAddressC.addressStore(variableInfo.offset)
      case LongTypeKey => StoreLongC.longStore(variableInfo.offset)
    }
  }

  override def transformGrammars(grammars: GrammarCatalogue, state: CompilationState): Unit = {
    val targetGrammar = grammars.find(AssignmentSkeleton.AssignmentTargetGrammar)
    val variableGrammar = grammars.find(VariableC.VariableGrammar)
    targetGrammar.addOption(variableGrammar)
  }

  override def description: String = "Enables assigning to a variable."
}
