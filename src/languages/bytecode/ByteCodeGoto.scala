package languages.bytecode
import ByteCode._
import transformation.{TransformationState, ProgramTransformation, MetaObject}

object ByteCodeGoTo extends ProgramTransformation {
  def goTo(target: String) = instruction("goTo", Seq(target))

  def ifIntegerCompareGreater(target: String) = instruction("integerCompareGreater", Seq(target))

  def label(name: String) = new MetaObject("label") {
    data.put("name",name)
  }


  def integerCompareGreater(target: String) = instruction("integerCompareGreater", Seq(target))

  def transform(program: MetaObject, state: TransformationState): Unit = ???

  def dependencies: Set[ProgramTransformation] = Set(ByteCode)
}
