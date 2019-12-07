package deltas.javac

import core.SourceUtils
import core.deltas.LanguageFromDeltas
import core.deltas.path.{NodePath, PathRoot}
import core.language.Compilation
import core.language.node.Node
import core.smarts._
import core.smarts.scopes.objects.Scope
import deltas.ConstraintSkeleton
import deltas.bytecode.readJar.ClassFileSignatureDecompiler
import deltas.javac.classes.ClassCompiler
import deltas.javac.classes.skeleton.PackageSignature

object JavaStandardLibrary {

  def getByteCodeNode(uri: String): Node = {
    val fileStream = SourceUtils.getResourceFile(uri)
    LanguageFromDeltas(ClassFileSignatureDecompiler.getDecompiler(uri, fileStream)).compile().program.asInstanceOf[NodePath].current
  }

  val systemClass: Node = getByteCodeNode("System.class")
  val printStreamClass: Node = getByteCodeNode("PrintStream.class")
  val objectClass: Node = getByteCodeNode("Object.class")
  val stringClass: Node = getByteCodeNode("String2.class")

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
