package deltas.javac

import core.deltas.{Delta, DeltaWithGrammar}
import deltas.bytecode.attributes._
import deltas.bytecode.constants._
import deltas.bytecode.coreInstructions._
import deltas.bytecode.coreInstructions.doubles.DoubleReturnInstructionDelta
import deltas.bytecode.coreInstructions.floats.FloatReturnInstructionDelta
import deltas.bytecode.coreInstructions.integers._
import deltas.bytecode.coreInstructions.integers.integerCompare._
import deltas.bytecode.coreInstructions.longs._
import deltas.bytecode.coreInstructions.objects._
import deltas.bytecode.extraConstants.TypeConstant
import deltas.bytecode.types._
import deltas.bytecode.{ByteCodeFieldInfo, ByteCodeMethodInfo, ByteCodeSkeleton}
import deltas.javac.types._

object ByteCodeLanguage {

  def byteCodeDeltas: Seq[Delta] = byteCodeInstructions ++ byteCodeWithoutInstructions

  def byteCodeInstructions: Seq[InstructionInstance] = {
    Seq(Pop2Delta, PopDelta, GetStaticDelta, GotoDelta, IfIntegerCompareLessDelta, IfIntegerCompareLessOrEqualDelta,
      IfZeroDelta, IfNotZero, InvokeSpecialDelta, InvokeVirtualDelta, InvokeStaticDelta, NewByteCodeDelta, Duplicate2InstructionDelta, DuplicateInstructionDelta) ++
      objectInstructions ++ Seq(PushNullDelta, StoreIntegerDelta, SubtractIntegerDelta, VoidReturnInstructionDelta,
      SwapInstruction, GetFieldDelta, PutField) ++
      integerInstructions ++ longInstructions ++ floatInstructions ++ doubleInstructions
  }

  def objectInstructions: Seq[InstructionInstance] = Seq(LoadAddressDelta, AddressReturnInstructionDelta, StoreAddressDelta)

  def doubleInstructions: Seq[InstructionInstance] = Seq(DoubleReturnInstructionDelta)

  def floatInstructions: Seq[InstructionInstance] = Seq(FloatReturnInstructionDelta)

  def longInstructions: Seq[InstructionInstance] = Seq(LongReturnInstructionDelta, AddLongsDelta, CompareLongDelta, PushLongDelta, LoadLongDelta, StoreLongDelta)

  def integerInstructions: Seq[InstructionInstance] = Seq(AddIntegersDelta, SmallIntegerConstantDelta,
    LoadConstantDelta, IncrementIntegerDelta, IntegerReturnInstructionDelta, LoadIntegerDelta, IfIntegerCompareGreaterOrEqualDelta,
    IfIntegerCompareEqualDelta, IfIntegerCompareNotEqualDelta)

  def byteCodeWithoutInstructions: Seq[Delta] = byteCodeWithoutTextualParser

  def byteCodeWithoutTextualParser: Seq[Delta] = bytecodeAttributes ++ constantEntryDeltas ++
    Seq(ByteCodeMethodInfo, ByteCodeFieldInfo) ++
    types ++ Seq(ByteCodeSkeleton)

  val bytecodeAttributes: Seq[DeltaWithGrammar] = Seq(StackMapTableAttributeDelta, LineNumberTable, SourceFileAttribute,
    CodeAttributeDelta, //ExceptionsAttribute, InnerClassesAttribute,
    SignatureAttribute)

  def constantEntryDeltas: Seq[Delta] = Seq(TypeConstant) ++
    Seq(MethodTypeConstant, Utf8ConstantDelta, DoubleInfoConstant, LongInfoConstant, FieldRefConstant,
      InterfaceMethodRefConstant, MethodRefConstant, NameAndTypeConstant,
      ClassInfoConstant, IntegerInfoConstant, StringConstant, MethodHandleConstant,
      InvokeDynamicConstant)

  def types: Seq[Delta] = Seq(SelectInnerClassDelta, TypeVariableDelta, TypeAbstraction, WildcardTypeArgument, ExtendsDelta,
    SuperTypeArgument, TypeApplicationDelta, MethodTypeDelta) ++
    Seq(UnqualifiedObjectTypeDelta, QualifiedObjectTypeDelta, ArrayTypeDelta, ByteTypeDelta, FloatTypeDelta,
      CharTypeDelta, BooleanTypeDelta, DoubleTypeDelta, LongTypeDelta, VoidTypeDelta, IntTypeDelta,
      ShortTypeDelta, TypeSkeleton)
}
