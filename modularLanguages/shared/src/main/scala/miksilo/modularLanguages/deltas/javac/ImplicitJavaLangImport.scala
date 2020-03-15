package miksilo.modularLanguages.deltas.javac

import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithPhase}
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.deltas.classes.ClassDelta.JavaClass
import miksilo.modularLanguages.deltas.javac.classes.WildcardImportDelta
import miksilo.modularLanguages.deltas.javac.classes.skeleton.JavaClassDelta

object ImplicitJavaLangImport extends DeltaWithPhase {
  val javaPackageName: String = "java"
  val langPackageName: String = "lang"

  override def dependencies: Set[Contract] = Set(JavaClassDelta, WildcardImportDelta)

  override def transformProgram(program: Node, state: Compilation): Unit = {
    val javaClass: JavaClass[Node] = program
    val imports = javaClass.imports
    val implicitImport = WildcardImportDelta.wildCardImport(Seq(javaPackageName, langPackageName))
    javaClass.imports = Seq(implicitImport) ++ imports
  }

  override def description: String = "Implicitly adds an import to java.lang"
}
