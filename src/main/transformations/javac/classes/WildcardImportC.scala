package transformations.javac.classes

import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import core.particles.{CompilationState, Contract, ParticleWithGrammar}
import transformations.javac.classes.BasicImportC._

object WildcardImportC extends ParticleWithGrammar {

  object WildcardImportKey

  def wildCardImport(elements: Seq[String]) = new Node(WildcardImportKey, ElementsKey -> elements)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val importPath = grammars.find(ImportPathGrammar)
    importPath.addOption((identifier.someSeparated(".") <~ ".*") ^^ parseMap(WildcardImportKey, ElementsKey))
  }

  override def inject(state: CompilationState): Unit = {
    JavaClassSkeleton.getState(state).importToClassMap.put(WildcardImportKey, wildcardImport => {
      val packageParts = getParts(wildcardImport)
      val classCompiler = JavaClassSkeleton.getState(state).classCompiler
      val compiler = classCompiler.compiler
      val finalPackage = compiler.find(packageParts).asInstanceOf[PackageInfo]

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
