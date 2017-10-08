package transformations.javac

import core.particles.CompilerFromDeltas
import core.particles.node.Node
import transformations.bytecode.readJar.ClassFileSignatureDecompiler
import transformations.javac.classes.ClassCompiler
import transformations.javac.classes.skeleton.{JavaCompilerState, PackageSignature}
import util.SourceUtils

object JavaLang {

  val compiler = new CompilerFromDeltas(ClassFileSignatureDecompiler.getDecompiler)

  val systemClass: Node = compiler.parseAndTransform(SourceUtils.getTestFile("System.class")).program
  val printStreamClass: Node = compiler.parseAndTransform(SourceUtils.getTestFile("PrintStream.class")).program
  val objectClass: Node = compiler.parseAndTransform(SourceUtils.getTestFile("Object.class")).program
  val stringClass: Node = compiler.parseAndTransform(SourceUtils.getTestFile("String2.class")).program

  def initialise(javaCompilerState: JavaCompilerState) {

    ClassCompiler(objectClass, javaCompilerState).bind()
    ClassCompiler(stringClass, javaCompilerState).bind()
    ClassCompiler(systemClass, javaCompilerState).bind()
    ClassCompiler(printStreamClass, javaCompilerState).bind()
  }

  val classPath = new PackageSignature(None, "")
}
