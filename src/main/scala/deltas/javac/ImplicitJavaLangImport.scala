package deltas.javac

import core.deltas.node.Node
import core.deltas.{Compilation, Contract, DeltaWithPhase}
import deltas.javac.classes.WildcardImportC
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.classes.skeleton.JavaClassSkeleton._

object ImplicitJavaLangImport extends DeltaWithPhase {
  val javaPackageName: String = "java"
  val langPackageName: String = "lang"

  override def dependencies: Set[Contract] = Set(JavaClassSkeleton, WildcardImportC)

  override def transformProgram(program: Node, state: Compilation): Unit = {
    val javaClass: JavaClass[Node] = program
    val imports = javaClass.imports
    val implicitImport = WildcardImportC.wildCardImport(Seq(javaPackageName, langPackageName))
    javaClass.imports = Seq(implicitImport) ++ imports
  }

  override def description: String = "Implicitly adds an import to java.lang"
}
