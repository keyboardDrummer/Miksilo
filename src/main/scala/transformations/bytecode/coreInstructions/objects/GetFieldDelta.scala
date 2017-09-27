package transformations.bytecode.coreInstructions.objects

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeClass, NodeField}
import core.particles.{Compilation, Language}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.constants.{FieldRefConstant, NameAndTypeConstant}
import transformations.bytecode.coreInstructions.{ConstantPoolIndexGrammar, InstructionDelta, InstructionSignature}
import transformations.bytecode.extraConstants.TypeConstant
import transformations.bytecode.simpleBytecode.ProgramTypeState

object GetFieldDelta extends InstructionDelta {

  override val key: Key = GetFieldKey
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
    val fieldRef = getField(FieldRef).asInstanceOf[Node]
    val nameAndType = fieldRef(FieldRefConstant.NameAndType).asInstanceOf[Node]
    val fieldType = TypeConstant.getValue(nameAndType(NameAndTypeConstant.Type).asInstanceOf[Node])
    fieldType
  }

  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).constantReferences.put(key, Map(FieldRef -> FieldRefConstant.key))
  }

  override def argumentsGrammar(grammars: GrammarCatalogue): BiGrammar = grammars.find(ConstantPoolIndexGrammar).as(FieldRef)

  override def getInstructionSize: Int = 3

  object GetFieldKey extends NodeClass

  override def grammarName = "getfield"
}
