package languages.java

import transformation.{TransformationState, MetaObject, ProgramTransformation}
import languages.bytecode.ByteCode
import languages.java.base.JavaBase

object ImplicitReturnAtEndOfMethod extends ProgramTransformation {
  override def dependencies: Set[ProgramTransformation] = Set(JavaBase)

  override def transform(program: MetaObject, state: TransformationState): Unit = {

//    if (!isInstructionSomeReturn(instructions.last))
//    {
//      instructions = instructions ++ Seq(ByteCode.voidReturn)
//    }
  }
}
