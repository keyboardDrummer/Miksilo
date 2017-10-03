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
  val stringClass: Node = compiler.parseAndTransform(SourceUtils.getTestFile("String2.class")).program //TODO move these vals to the object scope. However this seems to introduce a multi-threading problem, causing the test on the build server to fail.

  def initialise(javaCompilerState: JavaCompilerState) {
    new ClassCompiler(objectClass, javaCompilerState)
    new ClassCompiler(stringClass, javaCompilerState)
    new ClassCompiler(systemClass, javaCompilerState)
    new ClassCompiler(printStreamClass, javaCompilerState)
  }

  val classPath = new PackageSignature(None, "")
}
