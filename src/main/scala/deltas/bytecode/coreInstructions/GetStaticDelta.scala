package deltas.bytecode.coreInstructions

import core.bigrammar.BiGrammar
import core.deltas.Language
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeShape, NodeField}
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.constants.FieldRefConstant
import deltas.bytecode.constants.FieldRefConstant.FieldRefWrapper
import deltas.bytecode.simpleBytecode.ProgramTypeState

object GetStaticDelta extends InstructionDelta {

  object FieldRef extends NodeField

  def getStatic(fieldRefIndex: Any): Node = key.create(FieldRef -> fieldRefIndex)

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = instruction(FieldRef).asInstanceOf[Int]
    hexToBytes("b2") ++ shortToBytes(arguments)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(), Seq(getReturnType(instruction)))

  def getReturnType(getStatic: Node): Node = {
    val fieldRef: FieldRefWrapper[Node] = getStatic(FieldRef).asInstanceOf[Node]
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

  override def description: String = "Defines the getStatic instruction, which retrieves a value from a static field."

  override def grammarName = "getstatic"
}
