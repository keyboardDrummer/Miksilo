package transformations.bytecode.coreInstructions

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeClass, NodeField}
import core.particles.{Compilation, Language}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.constants.FieldRefConstant
import transformations.bytecode.constants.FieldRefConstant.FieldRefWrapper
import transformations.bytecode.simpleBytecode.ProgramTypeState

object GetStaticDelta extends InstructionDelta {

  override val key: Key = GetStaticKey
  object FieldRef extends NodeField

  def getStatic(fieldRefIndex: Any): Node = GetStaticKey.create(FieldRef -> fieldRefIndex)

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = instruction(FieldRef).asInstanceOf[Int]
    hexToBytes("b2") ++ shortToBytes(arguments)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature =
    new InstructionSignature(Seq(), Seq(getReturnType(instruction)))

  def getReturnType(getStatic: Node): Node = {
    val fieldRef: FieldRefWrapper[Node] = getStatic(FieldRef).asInstanceOf[Node]
    fieldRef.nameAndType._type.value
  }

  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).constantReferences.put(key, Map(FieldRef -> FieldRefConstant.key))
  }

  override def argumentsGrammar(grammars: GrammarCatalogue): BiGrammar = grammars.find(ConstantPoolIndexGrammar).as(FieldRef)

  override def getInstructionSize: Int = 3

  object GetStaticKey extends NodeClass

  override def description: String = "Defines the getStatic instruction, which retrieves a value from a static field."

  override def grammarName = "getstatic"
}
