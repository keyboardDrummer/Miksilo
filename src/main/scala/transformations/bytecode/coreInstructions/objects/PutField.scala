package transformations.bytecode.coreInstructions.objects

import core.particles.{Compilation, Language}
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeClass, NodeField}
import transformations.bytecode.constants.FieldRefConstant
import transformations.bytecode.coreInstructions.{ByteCodeTypeException, ConstantPoolIndexGrammar, InstructionDelta, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.{ByteCodeSkeleton, PrintByteCode}

object PutField extends InstructionDelta {

  object PutFieldKey extends NodeClass
  object FieldRef extends NodeField
  override val key = PutFieldKey

  def putField(index: Any) = PutFieldKey.create(FieldRef -> index)

  override def getInstructionSize: Int = 3
  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    PrintByteCode.hexToBytes("b5") ++ PrintByteCode.shortToBytes(instruction(FieldRef).asInstanceOf[Int])
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature = {
    val stackTop = typeState.stackTypes.takeRight(2)

    if (stackTop.size != 2)
      throw new ByteCodeTypeException("PutField requires two arguments on the stack.")

    val valueType = stackTop(1)
    val objectType = stackTop(0)

    assertObjectTypeStackTop(objectType, "PutField")

    new InstructionSignature(Seq.empty, Seq(valueType, objectType))
  }

  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getRegistry(state).constantReferences.put(key, Map(FieldRef -> FieldRefConstant.key))
  }

  override def argumentsGrammar(grammars: GrammarCatalogue) = grammars.find(ConstantPoolIndexGrammar).as(FieldRef)

  override def grammarName = "putfield"
}
