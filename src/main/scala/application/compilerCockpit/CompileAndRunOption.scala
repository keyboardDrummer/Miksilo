package application.compilerCockpit

import java.io.InputStream

import core.deltas._
import core.language.Compilation
import core.language.node.Node
import deltas.bytecode.ByteCodeSkeleton._
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

object CompileAndRunOption extends CompileOption {

  override def perform(cockpit: LanguageSandbox, inputStream: InputStream): TextWithGrammar = {
    val compiler = Delta.buildLanguage(cockpit.deltas ++ Seq(RunWithJVM))
    val state = compiler.parseAndTransform(inputStream)

    TextWithGrammar(state.output)
  }

  override def toString = "Compile and run"
}
