package application.compilerCockpit

import core.transformation.{Particle, CompilerFromParticles, CompilationState, MetaObject}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.constants.ClassRefConstant
import transformations.javac.classes.QualifiedClassName
import util.TestUtils

object RunWithJVM extends Particle
{
  override def inject(state: CompilationState): Unit = {
    state.compilerPhases ::= (() => {
      val clazz: MetaObject = state.program
      val classRefIndex = ByteCodeSkeleton.getClassNameIndex(clazz)
      val constantPool = ByteCodeSkeleton.getConstantPool(clazz)
      val classNameIndex = ClassRefConstant.getNameIndex(constantPool.getValue(classRefIndex).asInstanceOf[MetaObject])
      val className = constantPool.getValue(classNameIndex).asInstanceOf[QualifiedClassName].toString
      state.output = TestUtils.runByteCode(className, clazz)
    })
  }

  override def description: String = "Takes the bytecode program and runs it using the JVM."
}

object CompileAndRunOption extends CompileOption {

  override def perform(cockpit: CompilerCockpit, input: String): String = {
    val compiler = new CompilerFromParticles(cockpit.particles ++ Seq(RunWithJVM))
    val state = compiler.parseAndTransform(input)

    state.output
  }

  override def toString = "Compile and run"
}
