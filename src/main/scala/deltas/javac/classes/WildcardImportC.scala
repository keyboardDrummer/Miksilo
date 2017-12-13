package deltas.javac.classes

import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeShape}
import core.deltas.{Compilation, Contract, DeltaWithGrammar, Language}
import deltas.javac.classes.BasicImportC._
import deltas.javac.classes.skeleton.{JavaClassSkeleton, PackageSignature, QualifiedClassName}

object WildcardImportC extends DeltaWithGrammar {

  object WildcardImportKey extends NodeShape

  def wildCardImport(elements: Seq[String]) = new Node(WildcardImportKey, ElementsKey -> elements)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val importPath = find(ImportPathGrammar)
    importPath.addOption(identifier.someSeparated(".").as(ElementsKey) ~< ".*" asNode WildcardImportKey)
  }

  override def inject(state: Language): Unit = {
    JavaClassSkeleton.getRegistry(state).importToClassMap.put(WildcardImportKey, (compilation: Compilation, wildcardImport) => {
      val packageParts = getParts(wildcardImport)
      val classCompiler = JavaClassSkeleton.getState(compilation).classCompiler
      val compiler = classCompiler.javaCompiler
      val finalPackage = compiler.find(packageParts).asInstanceOf[PackageSignature]

      finalPackage.flattenContents().map(entry => {
        val className = entry._1.last
        val partiallyQualifiedClassName = entry._1
        className -> QualifiedClassName(packageParts ++ partiallyQualifiedClassName)
      }).toMap
    })
    super.inject(state)
  }

  override def dependencies: Set[Contract] = Set(BasicImportC)

  override def description: String = "Enables importing all classes from a package using a wildcard."
}
