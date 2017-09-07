package application.compilerCockpit

import java.io.InputStream

import core.particles.node.Node
import core.particles._
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.constants.{ClassRefConstant, QualifiedClassNameConstant}
import transformations.javac.classes.skeleton.QualifiedClassName
import util.TestUtils
import transformations.bytecode.ByteCodeSkeleton._

object RunWithJVM extends DeltaWithPhase
{
  override def transform(program: Node, state: CompilationState): Unit = {
    val clazz: Node = state.program
    val classRefIndex = ByteCodeSkeleton.getClassNameIndex(clazz)
    val constantPool = clazz.constantPool
    val classNameIndex = ClassRefConstant.getNameIndex(constantPool.getValue(classRefIndex).asInstanceOf[Node])
    val className = QualifiedClassNameConstant.get(constantPool.getValue(classNameIndex).asInstanceOf[Node]).toString
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
