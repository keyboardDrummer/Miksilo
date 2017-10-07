package transformations.javac

import core.particles.CompilerFromDeltas
import core.particles.node.Node
import transformations.bytecode.readJar.ClassFileSignatureDecompiler
import transformations.javac.classes.ClassCompiler
import transformations.javac.classes.skeleton.{JavaCompilerState, PackageSignature}
import util.SourceUtils

object JavaLang {

  var systemClass: Node = _
  var printStreamClass: Node = _
  var objectClass: Node = _
  var stringClass: Node = _

  this.synchronized {
    val compiler = new CompilerFromDeltas(ClassFileSignatureDecompiler.getDecompiler)
    systemClass = compiler.parseAndTransform(SourceUtils.getTestFile("System.class")).program
    printStreamClass = compiler.parseAndTransform(SourceUtils.getTestFile("PrintStream.class")).program
    objectClass = compiler.parseAndTransform(SourceUtils.getTestFile("Object.class")).program
    stringClass = compiler.parseAndTransform(SourceUtils.getTestFile("String2.class")).program
  }

  def initialise(javaCompilerState: JavaCompilerState) {
    this.synchronized {
      ClassCompiler(objectClass, javaCompilerState)
      ClassCompiler(stringClass, javaCompilerState)
      ClassCompiler(systemClass, javaCompilerState)
      ClassCompiler(printStreamClass, javaCompilerState)
    }
  }

  val classPath = new PackageSignature(None, "")
}
