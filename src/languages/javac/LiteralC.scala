package languages.javac

import transformation.{TransformationState, MetaObject, ProgramTransformation}
import languages.javac.base.JavaBase
import languages.bytecode.ByteCode

object LiteralC extends ProgramTransformation {
  def literal(value: AnyVal) = {
    new MetaObject(clazz) {
      data.put(valueKey, value)
    }
  }

  def getValue(literal: MetaObject) = { literal(valueKey) }
  val clazz = "literal"
  val valueKey = "value"
  def transform(program: MetaObject, state: TransformationState): Unit = {
    JavaBase.getStatementToLines(state).put(clazz,(literal : MetaObject, compiler) => {
      val value = getValue(literal)
      Seq(value match {
        case i:Integer => ByteCode.integerConstant(i)
        case b:Boolean => ByteCode.integerConstant(if (b) 1 else 0)
      })
    })
  }

  def dependencies: Set[ProgramTransformation] = Set(JavaBase)
}
