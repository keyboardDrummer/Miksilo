package transformations.javac

import core.particles.CompilerFromDeltas
import core.particles.node.Node
import transformations.bytecode.readJar.ClassFileSignatureDecompiler
import transformations.javac.classes.ClassCompiler
import transformations.javac.classes.skeleton.{JavaCompilerState, PackageSignature}
import util.SourceUtils

object JavaLang {

  val compiler = new CompilerFromDeltas(ClassFileSignatureDecompiler.getDecompiler)

  var systemClass: Node = _
  var printStreamClass: Node = _
  var objectClass: Node = _
  var stringClass: Node = _

  this.synchronized {
    systemClass = compiler.parseAndTransform(SourceUtils.getTestFile("System.class")).program
    printStreamClass = compiler.parseAndTransform(SourceUtils.getTestFile("PrintStream.class")).program
    objectClass = compiler.parseAndTransform(SourceUtils.getTestFile("Object.class")).program
    stringClass = compiler.parseAndTransform(SourceUtils.getTestFile("String2.class")).program
  }

  val systemHash = systemClass.hashCode()
  val printStreamHash = printStreamClass.hashCode()
  val objectHash = objectClass.hashCode()
  val stringHash = stringClass.hashCode()

  def initialise(javaCompilerState: JavaCompilerState) {
    if (systemHash != systemClass.hashCode())
      throw new Exception("system changed")
    if (printStreamHash != printStreamClass.hashCode())
      throw new Exception("printStreamClass changed")
    if (objectHash != objectClass.hashCode())
      throw new Exception("objectClass changed")
    if (stringHash != stringClass.hashCode())
      throw new Exception("stringClass changed")

    this.synchronized {
      ClassCompiler(objectClass, javaCompilerState)
      ClassCompiler(stringClass, javaCompilerState)
      ClassCompiler(systemClass, javaCompilerState)
      ClassCompiler(printStreamClass, javaCompilerState)
    }
  }

  val classPath = new PackageSignature(None, "")
}
