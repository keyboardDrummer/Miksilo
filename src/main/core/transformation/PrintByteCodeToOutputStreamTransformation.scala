package core.transformation

import java.io.OutputStream

import transformations.bytecode.PrintByteCode

class PrintByteCodeToOutputStreamTransformation(output: OutputStream) extends ProgramTransformation {

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    val bytes = PrintByteCode.getBytes(program, state).toArray
    output.write(bytes)
    output.close()
  }

  override def inject(state: TransformationState): Unit = {}

  override def dependencies: Set[Contract] = Set.empty
}
