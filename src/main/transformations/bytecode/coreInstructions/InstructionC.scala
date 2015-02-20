package transformations.bytecode.coreInstructions

import core.grammarDocument.BiGrammar
import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.bytecode.ByteCodeSkeleton.JumpBehavior
import transformations.bytecode._
import transformations.bytecode.attributes.{CodeAttribute, InstructionArgumentsKey}
import transformations.javac.classes.ConstantPool
import transformations.types.{LongTypeC, IntTypeC, TypeC}

case class InstructionSignature(inputs: Seq[MetaObject], outputs: Seq[MetaObject])

class ByteCodeTypeException(message: String) extends Exception(message)

object InstructionC
{
  def getInOutSizes(instruction: MetaObject, state: TransformationState): (Int,Int) = {
    val getSignature = ByteCodeSkeleton.getInstructionSignatureRegistry(state)(instruction.clazz)
    val constantPool = ByteCodeSkeleton.getConstantPool(state.program)
    try
    {
      val signature = getSignature(constantPool,instruction, Seq(IntTypeC.intType))
      getSignatureInOutLengths(state, signature)
    } catch
      {
        case e:ByteCodeTypeException =>
          val signature = getSignature(constantPool,instruction, Seq(LongTypeC.longType))
          getSignatureInOutLengths(state, signature)
      }
  }

  private def getSignatureInOutLengths(state: TransformationState, signature: InstructionSignature): (Int, Int) = {
    val inputLength = signature.inputs.map(_type => TypeC.getTypeSize(_type, state)).sum
    val outputLength = signature.outputs.map(_type => TypeC.getTypeSize(_type, state)).sum
    (inputLength, outputLength)
  }
}

trait InstructionC extends GrammarTransformation {

  override def inject(state: TransformationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getInstructionSignatureRegistry(state).put(key, (c,i, stackTypes) => getInstructionInAndOutputs(c, i, stackTypes, state))
    PrintByteCode.getBytesRegistry(state).put(key, getInstructionByteCode)
    ByteCodeSkeleton.getInstructionSizeRegistry(state).put(key, getInstructionSize)
    ByteCodeSkeleton.getState(state).jumpBehaviorRegistry.put(key, getJumpBehavior)
    ByteCodeSkeleton.getState(state).localUpdates.put(key, getVariableUpdates)
  }

  def assertDoubleWord(state: TransformationState, input: MetaObject): Unit = {
    if (TypeC.getTypeSize(input, state) != 2) {
      throw new ByteCodeTypeException("expected double word input")
    }
  }

  def assertSingleWord(state: TransformationState, input: MetaObject): Unit = {
    if (TypeC.getTypeSize(input, state) != 1) {
      throw new ByteCodeTypeException("expected single word input")
    }
  }

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  val key: AnyRef

  def getVariableUpdates(instruction: MetaObject): Map[Int, MetaObject] = Map.empty

  def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject,
                                 stackTypes: Seq[MetaObject], state: TransformationState): InstructionSignature

  def getInstructionSize(instruction: MetaObject): Int = getInstructionByteCode(instruction).size

  def getJumpBehavior: JumpBehavior = new JumpBehavior(true, false)

  def getInstructionByteCode(instruction: MetaObject): Seq[Byte]

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val instructionGrammar = grammars.find(CodeAttribute.InstructionGrammar)
    instructionGrammar.addOption(getGrammarForThisInstruction)
  }

  def getGrammarForThisInstruction: BiGrammar = {
    name ~> integer.manySeparated(",").inParenthesis ^^ parseMap(key, InstructionArgumentsKey)
  }

  protected def binary(_type: MetaObject) = InstructionSignature(Seq(_type, _type), Seq(_type))
}
