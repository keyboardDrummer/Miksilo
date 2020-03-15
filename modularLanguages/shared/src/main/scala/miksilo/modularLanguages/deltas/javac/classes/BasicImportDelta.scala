package miksilo.modularLanguages.deltas.javac.classes

import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.modularLanguages.core.node.{Node, NodeField, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.javac.classes.skeleton.{HasConstraintsDelta, JavaClassDelta, QualifiedClassName}

object BasicImportDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object Shape extends NodeShape

  object Elements extends NodeField

  def _import(elements: Seq[String]) = new Node(Shape, Elements -> elements)

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, _import: NodePath, parentScope: Scope): Unit = {
//    val elements = getParts(_import)
//    val fullPackage: String = elements.dropRight(1).fold("")((a, b) => a + "." + b)
//    val packageDeclaration = builder.resolve(fullPackage, _import.asInstanceOf[ChildPath], parentScope)
//    val packageScope = builder.resolveScopeDeclaration(packageDeclaration)
//    val classDeclaration = builder.resolve(elements.last, null, packageScope)
//    val classExternalScope = builder.resolveScopeDeclaration(classDeclaration)
//    builder.importScope(parentScope, classExternalScope) Eigenlijk een rename iets hier voor nodig.
    ???
  }

  override def shape: NodeShape = Shape

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val importPath = identifier.someSeparated(".").as(Elements).asLabelledNode(Shape)
    val basicImport = "import" ~~> importPath ~< ";"
    find(JavaClassDelta.ImportGrammar).addAlternative(basicImport)
  }

  def getParts(_import: Node) = _import(Elements).asInstanceOf[Seq[String]]

  override def inject(language: Language): Unit = {
    JavaClassDelta.importToClassMap.add(language, Shape, (compilation, _import) => {
      val elements = getParts(_import)
      val packageParts = elements.dropRight(1)
      val importedClassName = elements.last

      val qualifiedClassName = QualifiedClassName(packageParts ++ Seq(importedClassName))
      val result = Seq((importedClassName, qualifiedClassName)).toMap
      result
    })
    super.inject(language)
  }

  override def dependencies: Set[Contract] = Set(JavaClassDelta)

  override def description: String = "Allows importing a single class using an import statement."
}
