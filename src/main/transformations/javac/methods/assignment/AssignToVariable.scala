package transformations.javac.methods.assignment

import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.coreInstructions.integers.StoreIntegerC
import transformations.bytecode.coreInstructions.longs.StoreLongC
import transformations.bytecode.coreInstructions.objects.StoreAddressC
import transformations.javac.methods.{MethodC, VariableC, VariableInfo}
import transformations.types.ArrayTypeC.ArrayTypeKey
import transformations.types.IntTypeC.IntTypeKey
import transformations.types.LongTypeC.LongTypeKey
import transformations.types.ObjectTypeC.ObjectTypeKey
import transformations.types.TypeC

object AssignToVariable extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(AssignmentC, VariableC)

  override def inject(state: TransformationState): Unit = {
    AssignmentC.getState(state).assignFromStackByteCodeRegistry.put(VariableC.VariableKey, (targetVariable: MetaObject) => {
      val methodCompiler = MethodC.getMethodCompiler(state)
      val target = VariableC.getVariableName(targetVariable)
      val variableInfo = methodCompiler.variables(target)
      val byteCodeType = TypeC.toStackType(variableInfo._type, state)
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
    val targetGrammar = grammars.find(AssignmentC.AssignmentTargetGrammar)
    val variableGrammar = grammars.find(VariableC.VariableGrammar)
    targetGrammar.addOption(variableGrammar)
  }
}
