package deltas.bytecode.coreInstructions.objects

import core.deltas.grammars.LanguageGrammars
import core.language.node.{Node, NodeField}
import core.language.{Compilation, Language}
import deltas.bytecode.constants.FieldRefConstant
import deltas.bytecode.coreInstructions.{ByteCodeTypeException, ConstantPoolIndexGrammar, InstructionInstance, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.{ByteCodeSkeleton, PrintByteCode}

object PutField extends InstructionInstance {

  object FieldRef extends NodeField

  def putField(index: Any) = shape.create(FieldRef -> index)

  override def getInstructionSize(compilation: Compilation): Int = 3
  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
    PrintByteCode.hexToBytes("b5") ++ PrintByteCode.shortToBytes(instruction(FieldRef).asInstanceOf[Int])
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = {
    val stackTop = typeState.stackTypes.takeRight(2)

    if (stackTop.size != 2)
      throw new ByteCodeTypeException("PutField requires two arguments on the stack.")

    val valueType = stackTop(1)
    val objectType = stackTop(0)

    assertObjectTypeStackTop(objectType, "PutField")

    new InstructionSignature(Seq.empty, Seq(valueType, objectType))
  }

  override def inject(language: Language): Unit = {
    super.inject(language)
    ByteCodeSkeleton.constantReferences.add(language, shape, Map(FieldRef -> FieldRefConstant.shape))
  }

  override def argumentsGrammar(grammars: LanguageGrammars) = {
    import grammars._
    find(ConstantPoolIndexGrammar).as(FieldRef)
  }

  override def grammarName = "putfield"
}
