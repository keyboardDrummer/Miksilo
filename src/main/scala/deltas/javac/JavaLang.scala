package deltas.javac

import core.deltas.Compilation
import core.deltas.node.Node
import core.deltas.path.NodePathRoot
import core.language.Language
import core.nabl._
import deltas.bytecode.readJar.ClassFileSignatureDecompiler
import deltas.javac.classes.ClassCompiler
import deltas.javac.classes.skeleton.{JavaClassSkeleton, PackageSignature}
import util.SourceUtils

object JavaLang {

  val compiler = new Language(ClassFileSignatureDecompiler.getDecompiler)

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

  def getProofs(compilation: Compilation): Proofs = {
    val factory = new Factory()
    val builder = new ConstraintBuilder(factory)
    val compilationScope = builder.newScope()
    for(clazz <- Seq(objectClass, stringClass, systemClass, printStreamClass)) {
      JavaClassSkeleton.hasDeclarations.get(compilation, clazz.shape).getDeclaration(compilation, builder, NodePathRoot(clazz), //TODO hier moet ik eigenlijk even een package tree bouwen met alle classes er in.
        compilationScope)
    }
    val solver = new ConstraintSolver(builder, builder.getConstraints)
    solver.run()
    solver.proofs
  }
}
