package deltas.javac

import core.{SolveConstraintsDelta, SourceUtils}
import core.deltas.{Contract, Delta, LanguageFromDeltas}
import core.deltas.path.{NodePath, PathRoot}
import core.language.{Compilation, Language}
import core.language.node.Node
import core.smarts._
import core.smarts.scopes.objects.Scope
import deltas.ConstraintSkeleton
import deltas.bytecode.readJar.ClassFileSignatureDecompiler
import deltas.javac.classes.ClassCompiler

object JavaStandardLibraryDelta extends Delta {

  def getByteCodeNode(uri: String): Node = {
    val fileStream = SourceUtils.getResourceFile(uri)
    LanguageFromDeltas(ClassFileSignatureDecompiler.getDecompiler(uri, fileStream)).compile().program.asInstanceOf[NodePath].current
  }

  override def inject(language: Language): Unit = {

    SolveConstraintsDelta.constraintCollector.add(language, (compilation, builder) => {
      val defaultPackageScope = builder.newScope(None, "defaultPackageScope")
      val proofs = getProofs(compilation, builder.factory, defaultPackageScope)
      builder.proofs = proofs

      ConstraintSkeleton.constraints(
        compilation, builder, compilation.program.asInstanceOf[PathRoot], defaultPackageScope)
    })
    super.inject(language)
  }

  val systemClass: Node = getByteCodeNode("System.class")
  val printStreamClass: Node = getByteCodeNode("PrintStream.class")
  val objectClass: Node = getByteCodeNode("Object.class")
  val stringClass: Node = getByteCodeNode("String2.class")

  def loadIntoClassPath(compilation: Compilation): Unit = {
    ClassCompiler(objectClass, compilation).bind()
    ClassCompiler(stringClass, compilation).bind()
    ClassCompiler(systemClass, compilation).bind()
    ClassCompiler(printStreamClass, compilation).bind()
  }

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

  override def description: String = "Adds the Java standard library"

  override def dependencies: Set[Contract] = Set.empty
}
