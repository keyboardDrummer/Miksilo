package deltas.javac

import core.deltas.node.Node
import core.deltas.{Compilation, Contract, DeltaWithPhase, Language}
import deltas.javac.classes.WildcardImportC
import deltas.javac.classes.skeleton.JavaClassSkeleton
import JavaClassSkeleton._

object ImplicitJavaLangImport extends DeltaWithPhase {
  val javaPackageName: String = "java"
  val langPackageName: String = "lang"

  override def dependencies: Set[Contract] = Set(JavaClassSkeleton, WildcardImportC)

  override def transform(program: Node, state: Compilation): Unit = {
    val clazz = program
    val imports = clazz.imports
    val implicitImport = WildcardImportC.wildCardImport(Seq(javaPackageName, langPackageName))
    clazz.imports = Seq(implicitImport) ++ imports
  }


  override def description: String = "Implicitly adds an import to java.lang"
}
