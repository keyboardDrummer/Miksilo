package deltas

import core.deltas.path.PathRoot
import core.deltas.{Contract, DeltaWithPhase}
import core.language.Compilation
import core.language.node.Node
import deltas.bytecode.ByteCodeSkeleton.ClassFile
import deltas.bytecode.constants.ClassInfoConstant.ClassInfoConstantWrapper
import deltas.bytecode.constants.Utf8ConstantDelta.Utf8Constant
import deltas.bytecode.constants.{ClassInfoConstant, Utf8ConstantDelta}
import util.JavaSourceUtils

object RunWithJVM extends DeltaWithPhase
{
  override def transformProgram(program: Node, state: Compilation): Unit = {
    val classFile: ClassFile[Node] = state.program.asInstanceOf[PathRoot].current
    val classRefIndex = classFile.classInfoIndex
    val constantPool = classFile.constantPool
    val classNameIndex = new ClassInfoConstantWrapper(constantPool.getValue(classRefIndex).asInstanceOf[Node]).nameIndex
    val className = new Utf8Constant(constantPool.getValue(classNameIndex).asInstanceOf[Node]).value.toString
    state.output = JavaSourceUtils.runByteCode(className, classFile)
  }

  override def description: String = "Takes the bytecode program and runs it using the JVM."

  override def dependencies: Set[Contract] = Set[Contract](ClassInfoConstant, Utf8ConstantDelta)
}
