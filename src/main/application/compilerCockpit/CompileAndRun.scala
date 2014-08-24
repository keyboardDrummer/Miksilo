package application.compilerCockpit

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton
import transformations.javac.classes.{QualifiedClassName, ConstantPool}
import util.TestUtils

object CompileAndRun extends CompileOption {
  override def leave(state: TransformationState): Unit = {
    val clazz: MetaObject = state.program
    val classRefIndex = ByteCodeSkeleton.getClassNameIndex(clazz)
    val constantPool = new ConstantPool(ByteCodeSkeleton.getConstantPool(clazz))
    val classNameIndex = ByteCodeSkeleton.getClassRefName(constantPool.getValue(classRefIndex).asInstanceOf[MetaObject])
    val className = constantPool.getValue(classNameIndex).asInstanceOf[QualifiedClassName].toString
    val result = TestUtils.runByteCode(className, clazz)
    OutputOption.setOutput(state, result)
  }

  override def toString = "Compile and run"
}
