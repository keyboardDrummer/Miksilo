package transformations.javac.classes

import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass}
import core.particles.{Contract, DeltaWithGrammar, Language}
import transformations.javac.classes.BasicImportC._
import transformations.javac.classes.skeleton.{JavaClassSkeleton, PackageSignature, QualifiedClassName}

object WildcardImportC extends DeltaWithGrammar {

  object WildcardImportKey extends NodeClass

  def wildCardImport(elements: Seq[String]) = new Node(WildcardImportKey, ElementsKey -> elements)

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val importPath = grammars.find(ImportPathGrammar)
    importPath.addOption((identifier.someSeparated(".") <~ ".*").asNode(WildcardImportKey, ElementsKey))
  }

  override def inject(state: Language): Unit = {
    JavaClassSkeleton.getState(state).importToClassMap.put(WildcardImportKey, wildcardImport => {
      val packageParts = getParts(wildcardImport)
      val classCompiler = JavaClassSkeleton.getState(state).classCompiler
      val compiler = classCompiler.compiler
      val finalPackage = compiler.find(packageParts).asInstanceOf[PackageSignature]

      finalPackage.flattenContents().map(entry => {
        val className = entry._1.last
        val partiallyQualifiedClassName = entry._1
        className -> new QualifiedClassName(packageParts ++ partiallyQualifiedClassName)
      }).toMap
    })
    super.inject(state)
  }

  override def dependencies: Set[Contract] = Set(BasicImportC)

  override def description: String = "Enables importing all classes from a package using a wildcard."
}
