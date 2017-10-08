package transformations.javac

import core.particles.node.Node
import core.particles.{Compilation, CompilerFromDeltas}
import transformations.bytecode.readJar.ClassFileSignatureDecompiler
import transformations.javac.classes.ClassCompiler
import transformations.javac.classes.skeleton.PackageSignature
import util.SourceUtils

object JavaLang {

  val compiler = new CompilerFromDeltas(ClassFileSignatureDecompiler.getDecompiler)

  val systemClass: Node = compiler.parseAndTransform(SourceUtils.getTestFile("System.class")).program
  val printStreamClass: Node = compiler.parseAndTransform(SourceUtils.getTestFile("PrintStream.class")).program
  val objectClass: Node = compiler.parseAndTransform(SourceUtils.getTestFile("Object.class")).program
  val stringClass: Node = compiler.parseAndTransform(SourceUtils.getTestFile("String2.class")).program

  def loadIntoClassPath(compilation: Compilation) {
    ClassCompiler(objectClass, compilation).bind()
    ClassCompiler(stringClass, compilation).bind()
    ClassCompiler(systemClass, compilation).bind()
    ClassCompiler(printStreamClass, compilation).bind()
  }

  val classPath = new PackageSignature(None, "")
}
