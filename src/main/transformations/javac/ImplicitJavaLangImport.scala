package transformations.javac

import core.particles.node.Node
import core.particles.{CompilationState, Contract, ParticleWithPhase}
import transformations.javac.classes.{JavaClassSkeleton, WildcardImportC}

object ImplicitJavaLangImport extends ParticleWithPhase {
  override def dependencies: Set[Contract] = Set(JavaClassSkeleton, WildcardImportC)

  override def transform(program: Node, state: CompilationState): Unit = {
    val clazz = program
    val imports = JavaClassSkeleton.getImports(clazz)
    val implicitImport = WildcardImportC.wildCardImport(Seq(JavaLang.javaPackageName, JavaLang.langPackageName))
    clazz(JavaClassSkeleton.ClassImports) = Seq(implicitImport) ++ imports
  }

  override def description: String = "Implicitly adds an import to java.lang"
}
