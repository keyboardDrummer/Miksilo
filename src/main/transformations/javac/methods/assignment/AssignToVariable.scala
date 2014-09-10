package transformations.javac.methods.assignment

import core.transformation.{Contract, MetaObject, TransformationState}
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.bytecode.coreInstructions.integers.{LoadIntegerC, StoreIntegerC}
import transformations.bytecode.coreInstructions.longs.{LoadLongC, StoreLongC}
import transformations.bytecode.coreInstructions.objects.{LoadAddressC, StoreAddressC}
import transformations.javac.methods.{MethodC, VariableC}
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
      Seq(byteCodeType.clazz match {
        case IntTypeKey => StoreIntegerC.integerStore(variableInfo.offset)
        case ObjectTypeKey => StoreAddressC.addressStore(variableInfo.offset)
        case ArrayTypeKey => StoreAddressC.addressStore(variableInfo.offset)
        case LongTypeKey => StoreLongC.longStore(variableInfo.offset)
      }) ++ Seq(byteCodeType.clazz match {
        case IntTypeKey => LoadIntegerC.load(variableInfo.offset)
        case ObjectTypeKey => LoadAddressC.addressLoad(variableInfo.offset)
        case ArrayTypeKey => LoadAddressC.addressLoad(variableInfo.offset)
        case LongTypeKey => LoadLongC.load(variableInfo.offset)
      })
    })
    super.inject(state)
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val targetGrammar = grammars.find(AssignmentC.AssignmentTargetGrammar)
    val variableGrammar = grammars.find(VariableC.VariableGrammar)
    targetGrammar.addOption(variableGrammar)
  }
}
