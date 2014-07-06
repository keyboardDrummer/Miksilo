package transformations.javac

import core.transformation.{TransformationState, MetaObject, ProgramTransformation}
import transformations.javac.base.JavaBase
import transformations.bytecode.ByteCode

object LiteralC extends ProgramTransformation {
  def literal(value: AnyVal) = {
    new MetaObject(LiteralKey) {
      data.put(ValueKey, value)
    }
  }

  def getValue(literal: MetaObject) = { literal(ValueKey) }
  object LiteralKey
  object ValueKey
  def transform(program: MetaObject, state: TransformationState): Unit = {
    JavaBase.getStatementToLines(state).put(LiteralKey,(literal : MetaObject, compiler) => {
      val value = getValue(literal)
      Seq(value match {
        case i:Integer => ByteCode.integerConstant(i)
        case b:Boolean => ByteCode.integerConstant(if (b) 1 else 0)
      })
    })
  }

  def dependencies: Set[ProgramTransformation] = Set(JavaBase)
}
