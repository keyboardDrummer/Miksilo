package transformations.bytecode.coreInstructions.objects

import core.bigrammar.BiGrammar
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeClass, NodeField}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.constants.{FieldRefConstant, NameAndType}
import transformations.bytecode.coreInstructions.{ConstantPoolIndexGrammar, InstructionC, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool

object GetFieldC extends InstructionC {

  override val key: Key = GetFieldKey
  object FieldRef extends NodeField

  def construct(fieldRefIndex: Any): Node = GetFieldKey.create(FieldRef -> fieldRefIndex)

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    hexToBytes("b4") ++ shortToBytes(instruction(FieldRef).asInstanceOf[Int])
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
    val stackTop = typeState.stackTypes.last
    assertObjectTypeStackTop(stackTop, "getField")
    new InstructionSignature(Seq(stackTop), Seq(getReturnType(state.program.constantPool, instruction)))
  }

  def getReturnType(constantPool: ConstantPool, getField: Node): Node = {
    val fieldRefIndex = getField(FieldRef).asInstanceOf[Int]
    val fieldRef = constantPool.getValue(fieldRefIndex).asInstanceOf[Node]
    val nameAndType = constantPool.getValue(FieldRefConstant.getNameAndTypeIndex(fieldRef)).asInstanceOf[Node]
    val fieldType = constantPool.getValue(NameAndType.getTypeIndex(nameAndType)).asInstanceOf[Node]
    fieldType
  }

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).constantReferences.put(key, Map(FieldRef -> FieldRefConstant.key))
  }

  override def argumentsGrammar(grammars: GrammarCatalogue): BiGrammar = grammars.find(ConstantPoolIndexGrammar).as(FieldRef)

  override def getInstructionSize: Int = 3

  object GetFieldKey extends NodeClass
}
