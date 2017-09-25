package application.compilerCockpit

import java.io.InputStream

import core.particles._
import core.particles.node.Node
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.constants.ClassInfoConstant
import transformations.bytecode.extraConstants.QualifiedClassNameConstant
import util.TestUtils

object RunWithJVM extends DeltaWithPhase
{
  override def transform(program: Node, state: Compilation): Unit = {
    val clazz: Node = state.program
    val classRefIndex = ByteCodeSkeleton.getClassNameIndex(clazz)
    val constantPool = clazz.constantPool
    val classNameIndex = ClassInfoConstant.getNameIndex(constantPool.getValue(classRefIndex).asInstanceOf[Node])
    val className = QualifiedClassNameConstant.get(constantPool.getValue(classNameIndex).asInstanceOf[Node]).toString
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
