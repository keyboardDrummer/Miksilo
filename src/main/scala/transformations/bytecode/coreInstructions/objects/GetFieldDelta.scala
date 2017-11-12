package transformations.bytecode.coreInstructions.objects

import core.bigrammar.BiGrammar
import core.particles.grammars.LanguageGrammars
import core.particles.node.{Node, NodeClass, NodeField}
import core.particles.{Compilation, Language}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.constants.FieldRefConstant
import transformations.bytecode.constants.FieldRefConstant.FieldRefWrapper
import transformations.bytecode.coreInstructions.{ConstantPoolIndexGrammar, InstructionDelta, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState

object GetFieldDelta extends InstructionDelta {

  override val key = GetFieldKey
  object FieldRef extends NodeField

  def construct(fieldRefIndex: Any): Node = GetFieldKey.create(FieldRef -> fieldRefIndex)

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    hexToBytes("b4") ++ shortToBytes(instruction(FieldRef).asInstanceOf[Int])
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature = {
    val stackTop = typeState.stackTypes.last
    assertObjectTypeStackTop(stackTop, "getField")
    new InstructionSignature(Seq(stackTop), Seq(getReturnType(instruction)))
  }

  def getReturnType(getField: Node): Node = {
    val fieldRef: FieldRefWrapper[Node] = getField(FieldRef).asInstanceOf[Node]
    fieldRef.nameAndType._type.value
  }

  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getRegistry(state).constantReferences.put(key, Map(FieldRef -> FieldRefConstant.key))
  }

  override def argumentsGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    find(ConstantPoolIndexGrammar).as(FieldRef)
  }

  override def getInstructionSize: Int = 3

  object GetFieldKey extends NodeClass

  override def grammarName = "getfield"
}
