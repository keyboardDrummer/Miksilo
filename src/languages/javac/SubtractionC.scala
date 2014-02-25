package languages.javac

import transformation.{TransformationState, MetaObject, ProgramTransformation}
import languages.javac.base.JavaBase
import languages.bytecode.ByteCode

object SubtractionC extends ProgramTransformation {
  val clazz: String = "Subtraction"

  val firstKey: String = "first"

  val secondKey: String = "second"

  def subtraction(first: MetaObject, second: MetaObject) = new MetaObject(clazz) {
    data.put(firstKey, first)
    data.put(secondKey, second)
  }

  def getFirst(subtraction: MetaObject) = subtraction(firstKey).asInstanceOf[MetaObject]

  def getSecond(subtraction: MetaObject) = subtraction(secondKey).asInstanceOf[MetaObject]

  def transform(program: MetaObject, state: TransformationState): Unit = {
    JavaBase.getStatementToLines(state).put(clazz,(subtraction : MetaObject, compiler) => {
      val firstInstructions = JavaBase.statementToInstructions(getFirst(subtraction), compiler)
      val secondInstructions = JavaBase.statementToInstructions(getSecond(subtraction), compiler)
      firstInstructions ++ secondInstructions ++ Seq(ByteCode.subtractInteger)
    })
  }

  def dependencies: Set[ProgramTransformation] = Set(JavaBase)
}
