package deltas.javac.classes

import core.deltas.grammars.LanguageGrammars
import core.language.node.{Node, NodeShape}
import core.deltas.path.{NodeChildPath, NodePath}
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.javac.classes.skeleton.JavaClassDelta
import deltas.javac.classes.BasicImportDelta._
import deltas.javac.classes.skeleton._

object WildcardImportDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object Shape extends NodeShape

  def wildCardImport(elements: Seq[String]) = new Node(Shape, Elements -> elements)

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, _import: NodePath, parentScope: Scope): Unit = {
    val elements = getParts(_import)
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
      val packageParts = getParts(wildcardImport)
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

  override def description: String = "Enables importing all classes from a package using a wildcard."
}
