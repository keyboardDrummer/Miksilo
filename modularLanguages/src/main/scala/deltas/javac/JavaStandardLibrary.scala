package deltas.javac

import core.deltas.LanguageFromDeltas
import core.deltas.path.{AnyPath, NodePath, PathRoot}
import core.language.Compilation
import core.language.node.Node
import core.smarts._
import core.smarts.scopes.objects.Scope
import deltas.ConstraintSkeleton
import deltas.bytecode.readJar.ClassFileSignatureDecompiler
import deltas.javac.classes.ClassCompiler
import deltas.javac.classes.skeleton.PackageSignature
import util.JavaSourceUtils

object JavaStandardLibrary {

  val byteCodeDecompiler = LanguageFromDeltas(ClassFileSignatureDecompiler.getDecompiler)

  val systemClass: Node = byteCodeDecompiler.compileStream(JavaSourceUtils.getTestFile("System.class")).program.asInstanceOf[NodePath].current
  val printStreamClass: Node = byteCodeDecompiler.compileStream(JavaSourceUtils.getTestFile("PrintStream.class")).program.asInstanceOf[NodePath].current
  val objectClass: Node = byteCodeDecompiler.compileStream(JavaSourceUtils.getTestFile("Object.class")).program.asInstanceOf[NodePath].current
  val stringClass: Node = byteCodeDecompiler.compileStream(JavaSourceUtils.getTestFile("String2.class")).program.asInstanceOf[NodePath].current

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
      ConstraintSkeleton.hasDeclarations(compilation, clazz.shape).
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
