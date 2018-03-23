package application.compilerCockpit

import core.deltas.{Contract, DeltaWithPhase}
import core.language.Compilation
import core.language.node.Node
import deltas.bytecode.ByteCodeSkeleton.ClassFile
import deltas.bytecode.constants.ClassInfoConstant
import deltas.bytecode.constants.ClassInfoConstant.ClassInfoConstantWrapper
import deltas.bytecode.extraConstants.QualifiedClassNameConstantDelta
import deltas.bytecode.extraConstants.QualifiedClassNameConstantDelta.QualifiedClassNameConstant
import util.SourceUtils

object RunWithJVM extends DeltaWithPhase
{
  override def transformProgram(program: Node, state: Compilation): Unit = {
    val classFile: ClassFile[Node] = state.program
    val classRefIndex = classFile.classInfoIndex
    val constantPool = classFile.constantPool
    val classNameIndex = new ClassInfoConstantWrapper(constantPool.getValue(classRefIndex).asInstanceOf[Node]).nameIndex
    val className = new QualifiedClassNameConstant(constantPool.getValue(classNameIndex).asInstanceOf[Node]).value.toString
    state.output = SourceUtils.runByteCode(className, classFile)
  }

  override def description: String = "Takes the bytecode program and runs it using the JVM."

  override def dependencies: Set[Contract] = Set[Contract](ClassInfoConstant, QualifiedClassNameConstantDelta)
}
