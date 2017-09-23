package transformations.bytecode.coreInstructions

import core.bigrammar.BiGrammar
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeClass, NodeField}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.constants.{FieldRefConstant, NameAndTypeConstant}
import transformations.bytecode.extraConstants.TypeConstant
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool

object GetStaticDelta$ extends InstructionDelta {

  override val key: Key = GetStaticKey
  object FieldRef extends NodeField

  def getStatic(fieldRefIndex: Any): Node = GetStaticKey.create(FieldRef -> fieldRefIndex)

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = instruction(FieldRef).asInstanceOf[Int]
    hexToBytes("b2") ++ shortToBytes(arguments)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature =
    new InstructionSignature(Seq(), Seq(getReturnType(state.program.constantPool, instruction)))

  def getReturnType(constantPool: ConstantPool, getStatic: Node): Node = {
    val location = getStatic(FieldRef).asInstanceOf[Int]
    val fieldRef = constantPool.getValue(location).asInstanceOf[Node]
    val nameAndType = constantPool.getValue(FieldRefConstant.getNameAndTypeIndex(fieldRef)).asInstanceOf[Node]
    val fieldType = TypeConstant.getValue(constantPool.getValue(NameAndTypeConstant.getTypeIndex(nameAndType)).asInstanceOf[Node])
    fieldType
  }


  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).constantReferences.put(key, Map(FieldRef -> FieldRefConstant.key))
  }

  override def argumentsGrammar(grammars: GrammarCatalogue): BiGrammar = grammars.find(ConstantPoolIndexGrammar).as(FieldRef)

  override def getInstructionSize: Int = 3

  object GetStaticKey extends NodeClass

  override def description: String = "Defines the getStatic instruction, which retrieves a value from a static field."
}
