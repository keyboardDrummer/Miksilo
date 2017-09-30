package application.compilerCockpit

import java.io.InputStream

import core.particles._
import core.particles.node.Node
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.constants.ClassInfoConstant.ClassInfoConstantWrapper
import transformations.bytecode.extraConstants.QualifiedClassNameConstantDelta.QualifiedClassNameConstant
import util.TestUtils

object RunWithJVM extends DeltaWithPhase
{
  override def transform(program: Node, state: Compilation): Unit = {
    val clazz: ByteCodeWrapper[Node] = state.program
    val classRefIndex = clazz.classInfoIndex
    val constantPool = clazz.constantPool
    val classNameIndex = new ClassInfoConstantWrapper(constantPool.getValue(classRefIndex).asInstanceOf[Node]).nameIndex
    val className = new QualifiedClassNameConstant(constantPool.getValue(classNameIndex).asInstanceOf[Node]).value.toString
    state.output = TestUtils.runByteCode(className, clazz)
  }

  override def description: String = "Takes the bytecode program and runs it using the JVM."
}

object CompileAndRunOption extends CompileOption {

  override def perform(cockpit: CompilerCockpit, inputStream: InputStream): TextWithGrammar = {
    val compiler = new CompilerFromDeltas(cockpit.particles ++ Seq(RunWithJVM))
    val state = compiler.parseAndTransform(inputStream)

    TextWithGrammar(state.output)
  }

  override def toString = "Compile and run"
}
