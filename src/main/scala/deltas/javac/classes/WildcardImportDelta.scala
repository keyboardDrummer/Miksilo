package deltas.javac.classes

import core.deltas.grammars.LanguageGrammars
import core.deltas.node.Node
import core.deltas.path.NodePath
import core.deltas.{Compilation, Contract, DeltaWithGrammar}
import core.language.Language
import core.nabl.ConstraintBuilder
import core.nabl.scopes.objects.Scope
import deltas.javac.classes.BasicImportDelta._
import deltas.javac.classes.skeleton.{JavaClassSkeleton, PackageSignature, QualifiedClassName, ShapeWithConstraints}

object WildcardImportDelta extends DeltaWithGrammar {

  object WildcardImportKey extends ShapeWithConstraints {
    override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, _import: NodePath, parentScope: Scope): Unit = {

      val elements = getParts(_import)
      val fullPackage: String = elements.reduce((a, b) => a + "." + b)
      val packageDeclaration = builder.resolve(fullPackage, _import, parentScope)
      val packageScope = builder.resolveScopeDeclaration(packageDeclaration)
      builder.importScope(parentScope, packageScope)
    }
  }

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

  override def dependencies: Set[Contract] = Set(BasicImportDelta)

  override def description: String = "Enables importing all classes from a package using a wildcard."
}
