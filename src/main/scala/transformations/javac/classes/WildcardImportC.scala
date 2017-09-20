package transformations.javac.classes

import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import core.particles.{CompilationState, Contract, DeltaWithGrammar}
import transformations.javac.classes.BasicImportC._
import transformations.javac.classes.skeleton.{JavaClassSkeleton, PackageSignature, QualifiedClassName}

object WildcardImportC extends DeltaWithGrammar {

  object WildcardImportKey extends Key

  def wildCardImport(elements: Seq[String]) = new Node(WildcardImportKey, ElementsKey -> elements)

  override def transformGrammars(grammars: GrammarCatalogue, state: CompilationState): Unit = {
    val importPath = grammars.find(ImportPathGrammar)
    importPath.addOption((identifier.someSeparated(".") <~ ".*").asNode(WildcardImportKey, ElementsKey))
  }

  override def inject(state: CompilationState): Unit = {
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
