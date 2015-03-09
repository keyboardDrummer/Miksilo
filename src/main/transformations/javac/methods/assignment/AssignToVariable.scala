package transformations.javac.methods.assignment

import core.particles.grammars.GrammarCatalogue
import core.particles.{CompilationState, Contract, MetaObject, ParticleWithGrammar}
import transformations.bytecode.coreInstructions.integers.StoreIntegerC
import transformations.bytecode.coreInstructions.longs.StoreLongC
import transformations.bytecode.coreInstructions.objects.StoreAddressC
import transformations.javac.methods.{MethodC, VariableC, VariableInfo}
import transformations.types.ArrayTypeC.ArrayTypeKey
import transformations.types.IntTypeC.IntTypeKey
import transformations.types.LongTypeC.LongTypeKey
import transformations.types.ObjectTypeC.ObjectTypeKey
import transformations.types.TypeSkeleton

object AssignToVariable extends ParticleWithGrammar {

  override def dependencies: Set[Contract] = Set(AssignmentSkeleton, VariableC)

  override def inject(state: CompilationState): Unit = {
    AssignmentSkeleton.getState(state).assignFromStackByteCodeRegistry.put(VariableC.VariableKey, (targetVariable: MetaObject) => {
      val methodCompiler = MethodC.getMethodCompiler(state)
      val target = VariableC.getVariableName(targetVariable)
      val variableInfo = methodCompiler.variables(target)
      val byteCodeType = TypeSkeleton.toStackType(variableInfo._type, state)
      Seq(getStoreInstruction(variableInfo, byteCodeType))
    })
    super.inject(state)
  }

  def getStoreInstruction(variableInfo: VariableInfo, byteCodeType: MetaObject): MetaObject = {
    byteCodeType.clazz match {
      case IntTypeKey => StoreIntegerC.integerStore(variableInfo.offset)
      case ObjectTypeKey => StoreAddressC.addressStore(variableInfo.offset)
      case ArrayTypeKey => StoreAddressC.addressStore(variableInfo.offset)
      case LongTypeKey => StoreLongC.longStore(variableInfo.offset)
    }
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val targetGrammar = grammars.find(AssignmentSkeleton.AssignmentTargetGrammar)
    val variableGrammar = grammars.find(VariableC.VariableGrammar)
    targetGrammar.addOption(variableGrammar)
  }

  override def description: String = "Enables assigning to a variable."
}
