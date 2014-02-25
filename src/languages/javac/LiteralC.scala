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

  def getValue(literal: MetaObject) = { literal(valueKey).asInstanceOf[Integer] }
  val clazz = "literal"
  val valueKey = "value"
  def transform(program: MetaObject, state: TransformationState): Unit = {
    JavaBase.getStatementToLines(state).put(clazz,(literal : MetaObject, compiler) => {
      val integer = getValue(literal)
      Seq(ByteCode.integerConstant(integer))
    })
  }

  def dependencies: Set[ProgramTransformation] = Set(JavaBase)
}
