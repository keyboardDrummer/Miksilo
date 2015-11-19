package application.compilerCockpit

import java.io.InputStream

import core.particles.node.Node
import core.particles._
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.constants.ClassRefConstant
import transformations.javac.classes.skeleton.QualifiedClassName
import util.TestUtils

object RunWithJVM extends ParticleWithPhase
{
  override def transform(program: Node, state: CompilationState): Unit = {
    val clazz: Node = state.program
    val classRefIndex = ByteCodeSkeleton.getClassNameIndex(clazz)
    val constantPool = ByteCodeSkeleton.getConstantPool(clazz)
    val classNameIndex = ClassRefConstant.getNameIndex(constantPool.getValue(classRefIndex).asInstanceOf[Node])
    val className = constantPool.getValue(classNameIndex).asInstanceOf[QualifiedClassName].toString
    state.output = TestUtils.runByteCode(className, clazz)
  }

  override def description: String = "Takes the bytecode program and runs it using the JVM."
}

object CompileAndRunOption extends CompileOption {

  override def perform(cockpit: CompilerCockpit, inputStream: InputStream): TextWithGrammar = {
    val compiler = new CompilerFromParticles(cockpit.particles ++ Seq(RunWithJVM))
    val state = compiler.parseAndTransform(inputStream)

    TextWithGrammar(state.output)
  }

  override def toString = "Compile and run"
}
