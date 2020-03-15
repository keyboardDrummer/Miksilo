package miksilo.modularLanguages.deltas

import miksilo.modularLanguages.core.deltas.path.PathRoot
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithPhase}
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.bytecode.ByteCodeSkeleton.ClassFile
import miksilo.modularLanguages.deltas.bytecode.constants.ClassInfoConstant.ClassInfoConstantWrapper
import miksilo.modularLanguages.deltas.bytecode.constants.Utf8ConstantDelta.Utf8Constant
import miksilo.modularLanguages.deltas.bytecode.constants.{ClassInfoConstant, Utf8ConstantDelta}
import miksilo.modularLanguages.util.JavaSourceUtils

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
