package application.compilerCockpit

import java.io.InputStream

import core.deltas._
import core.deltas.node.Node
import deltas.bytecode.ByteCodeSkeleton._
import deltas.bytecode.constants.ClassInfoConstant.ClassInfoConstantWrapper
import deltas.bytecode.extraConstants.QualifiedClassNameConstantDelta.QualifiedClassNameConstant
import util.SourceUtils

object RunWithJVM extends DeltaWithPhase
{
  override def transformProgram(program: Node, state: Compilation): Unit = {
    val clazz: ClassFile[Node] = state.program
    val classRefIndex = clazz.classInfoIndex
    val constantPool = clazz.constantPool
    val classNameIndex = new ClassInfoConstantWrapper(constantPool.getValue(classRefIndex).asInstanceOf[Node]).nameIndex
    val className = new QualifiedClassNameConstant(constantPool.getValue(classNameIndex).asInstanceOf[Node]).value.toString
    state.output = SourceUtils.runByteCode(className, clazz)
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
