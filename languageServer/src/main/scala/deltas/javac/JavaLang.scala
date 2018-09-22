package deltas.javac

import core.deltas.Delta
import core.deltas.path.PathRoot
import core.language.Compilation
import core.language.node.Node
import core.smarts._
import core.smarts.scopes.objects.Scope
import deltas.ConstraintSkeleton
import deltas.bytecode.readJar.ClassFileSignatureDecompiler
import deltas.javac.classes.ClassCompiler
import deltas.javac.classes.skeleton.PackageSignature
import util.SourceUtils

object JavaLang {

  val byteCodeDecompiler = Delta.buildLanguage(ClassFileSignatureDecompiler.getDecompiler)

  val systemClass: Node = byteCodeDecompiler.parseAndTransform(SourceUtils.getTestFile("System.class")).program
  val printStreamClass: Node = byteCodeDecompiler.parseAndTransform(SourceUtils.getTestFile("PrintStream.class")).program
  val objectClass: Node = byteCodeDecompiler.parseAndTransform(SourceUtils.getTestFile("Object.class")).program
  val stringClass: Node = byteCodeDecompiler.parseAndTransform(SourceUtils.getTestFile("String2.class")).program

  def loadIntoClassPath(compilation: Compilation) {
    ClassCompiler(objectClass, compilation).bind()
    ClassCompiler(stringClass, compilation).bind()
    ClassCompiler(systemClass, compilation).bind()
    ClassCompiler(printStreamClass, compilation).bind()
  }

  val classPath = new PackageSignature(None, "")

  def getProofs(compilation: Compilation, factory: Factory, scope: Scope): Proofs = {
    val builder = new ConstraintBuilder(factory)
    for(clazz <- Seq(objectClass, stringClass, systemClass, printStreamClass)) {
      ConstraintSkeleton.hasDeclarations.get(compilation, clazz.shape).
        getDeclaration(compilation, builder, PathRoot(clazz), scope)
    }
    val solver = new ConstraintSolver(builder, builder.getConstraints)
    try {
      solver.run()
    } catch {
      case CouldNotApplyConstraints(e) =>
    }
    solver.proofs
  }
}
