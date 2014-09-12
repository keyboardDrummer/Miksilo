package transformations.javac.classes

import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.javac.classes.BasicImportC._

object WildcardImportC extends GrammarTransformation {

  object WildcardImportKey

  def wildCardImport(elements: Seq[String]) = new MetaObject(WildcardImportKey, ElementsKey -> elements)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val importPath = grammars.find(ImportPathGrammar)
    importPath.addOption((identifier.someSeparated(".") <~ ".*") ^^ parseMap(WildcardImportKey, ElementsKey))
  }

  override def inject(state: TransformationState): Unit = {
    ClassC.getState(state).importToClassMap.put(WildcardImportKey, wildcardImport => {
      val packageParts = getParts(wildcardImport)
      val classCompiler = ClassC.getState(state).classCompiler
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
}
