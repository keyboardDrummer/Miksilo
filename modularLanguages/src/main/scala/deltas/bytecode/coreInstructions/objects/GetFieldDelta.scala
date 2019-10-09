package deltas.bytecode.coreInstructions.objects

import core.deltas.grammars.LanguageGrammars
import core.language.node.{Node, NodeField}
import core.language.{Compilation, Language}
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.constants.FieldRefConstant
import deltas.bytecode.constants.FieldRefConstant.FieldRefWrapper
import deltas.bytecode.coreInstructions.{ConstantPoolIndexGrammar, InstructionInstance, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState

object GetFieldDelta extends InstructionInstance {

  object FieldRef extends NodeField

  def construct(fieldRefIndex: Any): Node = shape.create(FieldRef -> fieldRefIndex)

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
    hexToBytes("b4") ++ shortToBytes(instruction(FieldRef).asInstanceOf[Int])
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = {
    val stackTop = typeState.stackTypes.last
    assertObjectTypeStackTop(stackTop, "getField")
    new InstructionSignature(Seq(stackTop), Seq(getReturnType(instruction)))
  }

  def getReturnType(getField: Node): Node = {
    val fieldRef: FieldRefWrapper[Node] = getField(FieldRef).asInstanceOf[Node]
    fieldRef.nameAndType._type.value
  }

  override def inject(language: Language): Unit = {
    super.inject(language)
    ByteCodeSkeleton.constantReferences.add(language, shape, Map(FieldRef -> FieldRefConstant.shape))
  }

  override def argumentsGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    find(ConstantPoolIndexGrammar).as(FieldRef)
  }

  override def getInstructionSize(compilation: Compilation): Int = 3

  override def grammarName = "getfield"
}
