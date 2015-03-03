package transformations.bytecode

import core.transformation.{TransformationState, MetaObject}
import transformations.bytecode.attributes.{StackMapTableAttribute, LineNumberTable, CodeAttribute}
import PrintByteCode._
import transformations.types.TypeC

object ByteCodeAttributes {

  def getAttributesByteCode(clazz: MetaObject, state: TransformationState, attributes: Seq[MetaObject]): Seq[Byte] = {

    def getAttributeByteCode(attribute: MetaObject): Seq[Byte] = {
      shortToBytes(ByteCodeSkeleton.getAttributeNameIndex(attribute)) ++
        prefixWithIntLength(() => attribute.clazz match {
          case CodeAttribute.CodeKey =>
            getCodeAttributeBytes(attribute)
          case LineNumberTable.LineNumberTableKey =>
            getLineNumberTableBytes(attribute)
          case StackMapTableAttribute.StackMapTableKey => getStackMapTableBytes(attribute)
          case ByteCodeSkeleton.SourceFileAttribute => getSourceFileBytes(attribute)
        })
    }

    def getFrameByteCode(frame: MetaObject): Seq[Byte] = {
      val offset = StackMapTableAttribute.getFrameOffset(frame)
      frame.clazz match {
        case StackMapTableAttribute.SameFrameKey =>
          if (offset > 63)
            byteToBytes(251) ++ shortToBytes(offset)
          else
            byteToBytes(offset)
        case StackMapTableAttribute.AppendFrame =>
          val localVerificationTypes = StackMapTableAttribute.getAppendFrameTypes(frame)
          byteToBytes(252 + localVerificationTypes.length - 1) ++
            shortToBytes(offset) ++ localVerificationTypes.flatMap(info => TypeC.getVerificationInfoBytes(clazz, info, state))
        case StackMapTableAttribute.SameLocals1StackItem =>
          val _type = StackMapTableAttribute.getSameLocals1StackItemType(frame)
          val code = 64 + offset
          if (code <= 127) {
            byteToBytes(code) ++ TypeC.getVerificationInfoBytes(clazz, _type, state)
          } else {
            byteToBytes(247) ++ shortToBytes(offset) ++ TypeC.getVerificationInfoBytes(clazz, _type, state)
          }
      }
    }

    def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
      ByteCodeSkeleton.getState(state).getBytes(instruction.clazz)(instruction)
    }

    def getStackMapTableBytes(attribute: MetaObject): Seq[Byte] = {
      val entries = StackMapTableAttribute.getStackMapTableEntries(attribute)
      shortToBytes(entries.length) ++ entries.flatMap(getFrameByteCode)
    }

    def getCodeAttributeBytes(attribute: MetaObject): Seq[Byte] = {
      val exceptionTable = CodeAttribute.getCodeExceptionTable(attribute)
      shortToBytes(CodeAttribute.getCodeMaxStack(attribute)) ++
        shortToBytes(CodeAttribute.getCodeMaxLocals(attribute)) ++
        prefixWithIntLength(() => CodeAttribute.getCodeInstructions(attribute).flatMap(getInstructionByteCode)) ++
        shortToBytes(exceptionTable.length) ++
        exceptionTable.flatMap(exception => getExceptionByteCode(exception)) ++
        getAttributesByteCode(clazz, state, CodeAttribute.getCodeAttributes(attribute))
    }

    shortToBytes(attributes.length) ++ attributes.flatMap(attribute => getAttributeByteCode(attribute))
  }
}
