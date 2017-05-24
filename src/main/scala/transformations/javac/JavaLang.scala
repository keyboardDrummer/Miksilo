package transformations.javac

import core.particles.CompilerFromParticles
import core.particles.node.Node
import transformations.bytecode.readJar.ClassFileSignatureDecompiler
import transformations.javac.classes.ClassCompiler
import transformations.javac.classes.skeleton.{MyCompiler, PackageSignature}
import util.TestUtils

object JavaLang {


  def initialise(compiler2: MyCompiler) {
    val compiler = new CompilerFromParticles(ClassFileSignatureDecompiler.getDecompiler)
    val systemClass: Node = compiler.parseAndTransform(TestUtils.getTestFile("System.class")).program
    val printStreamClass: Node = compiler.parseAndTransform(TestUtils.getTestFile("PrintStream.class")).program
    val objectClass: Node = compiler.parseAndTransform(TestUtils.getTestFile("Object.class")).program
    val stringClass: Node = compiler.parseAndTransform(TestUtils.getTestFile("String.class")).program
    
    new ClassCompiler(objectClass, compiler2)
    new ClassCompiler(stringClass, compiler2)
    new ClassCompiler(systemClass, compiler2)
    new ClassCompiler(printStreamClass, compiler2)
  }

  val classPath = new PackageSignature(None, "")
}
