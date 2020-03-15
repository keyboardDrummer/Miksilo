package miksilo.modularLanguages.deltas.javac.classes

import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.node.{Node, NodeShape}
import miksilo.modularLanguages.core.deltas.path.{NodeChildPath, NodePath}
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.classes.HasConstraintsDelta
import miksilo.modularLanguages.deltas.javac.classes.BasicImportDelta.Elements
import miksilo.modularLanguages.deltas.javac.classes.skeleton.{JavaClassDelta, PackageSignature, QualifiedClassName}

object WildcardImportDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object Shape extends NodeShape

  def wildCardImport(elements: Seq[String]) = new Node(Shape, Elements -> elements)

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, _import: NodePath, parentScope: Scope): Unit = {
    val elements = BasicImportDelta.getParts(_import)
    val fullPackage: String = elements.reduce((a, b) => a + "." + b)
    val packageDeclaration = builder.resolve(fullPackage, parentScope, _import.asInstanceOf[NodeChildPath])
    val packageScope = builder.getDeclaredScope(packageDeclaration)
    builder.importScope(parentScope, packageScope)
  }

  override def shape: NodeShape = Shape

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val importPath = find(BasicImportDelta.Shape)
    importPath.addAlternative(identifier.someSeparated(".").as(Elements) ~< ".*" asNode Shape)
  }

  override def inject(language: Language): Unit = {
    JavaClassDelta.importToClassMap.add(language, Shape, (compilation: Compilation, wildcardImport) => {
      val packageParts = BasicImportDelta.getParts(wildcardImport)
      val classCompiler = JavaClassDelta.state(compilation).classCompiler
      val compiler = classCompiler.javaCompiler
      val finalPackage = compiler.find(packageParts).asInstanceOf[PackageSignature]

      finalPackage.flattenContents().map(entry => {
        val className = entry._1.last
        val partiallyQualifiedClassName = entry._1
        className -> QualifiedClassName(packageParts ++ partiallyQualifiedClassName)
      }).toMap
    })
    super.inject(language)
  }

  override def dependencies: Set[Contract] = Set(BasicImportDelta)

  override def description: String = "Enables importing all classes from a package miksilo.modularLanguagesusing a wildcard."
}
