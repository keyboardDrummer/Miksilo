package languages.java

import transformation.{TransformationState, MetaObject, ProgramTransformation}
import languages.java.base.JavaBase
import languages.bytecode.ByteCode

object AdditionC extends ProgramTransformation {
  val clazz: String = "Addition"

  val firstKey: String = "first"

  val secondKey: String = "second"

  def addition(first: MetaObject, second: MetaObject) = new MetaObject(clazz) {
    data.put(firstKey, first)
    data.put(secondKey, second)
  }

  def getFirst(addition: MetaObject) = addition(firstKey).asInstanceOf[MetaObject]

  def getSecond(addition: MetaObject) = addition(secondKey).asInstanceOf[MetaObject]

  def transform(program: MetaObject, state: TransformationState): Unit = {
    JavaBase.getStatementToLines(state).put(clazz,(addition : MetaObject, compiler) => {
      val firstInstructions = JavaBase.statementToInstructions(getFirst(addition), compiler)
      val secondInstructions = JavaBase.statementToInstructions(getSecond(addition), compiler)
      firstInstructions ++ secondInstructions ++ Seq(ByteCode.addInteger)
    })
  }

  def dependencies: Set[ProgramTransformation] = Set(JavaBase)
}
