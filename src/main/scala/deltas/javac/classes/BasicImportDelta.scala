package deltas.javac.classes

import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{GrammarKey, Node, NodeField}
import core.deltas.path.{ChildPath, NodePath}
import core.deltas.{Compilation, Contract, DeltaWithGrammar}
import core.language.Language
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.javac.classes.skeleton.{JavaClassSkeleton, QualifiedClassName, ShapeWithConstraints}

object BasicImportDelta extends DeltaWithGrammar {

  object ImportKey extends ShapeWithConstraints {
    override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, _import: NodePath, parentScope: Scope): Unit = {

      val elements = getParts(_import)
      val fullPackage: String = elements.dropRight(1).fold("")((a, b) => a + "." + b)
      val packageDeclaration = builder.resolve(fullPackage, _import.asInstanceOf[ChildPath], parentScope)
      val packageScope = builder.resolveScopeDeclaration(packageDeclaration)
      val classDeclaration = builder.resolve(elements.last, null, packageScope)
      val classExternalScope = builder.resolveScopeDeclaration(classDeclaration)
      builder.importScope(parentScope, classExternalScope)
    }
  }

  object ElementsKey extends NodeField

  object ImportPathGrammar extends GrammarKey

  def _import(elements: Seq[String]) = new Node(ImportKey, ElementsKey -> elements)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val importPath = create(ImportPathGrammar, identifier.someSeparated(".").as(ElementsKey).asNode(ImportKey))
    val basicImport = "import" ~~> importPath ~< ";"
    find(JavaClassSkeleton.ImportGrammar).addOption(basicImport)
  }

  def getParts(_import: Node) = _import(ElementsKey).asInstanceOf[Seq[String]]

  override def inject(state: Language): Unit = {
    JavaClassSkeleton.getRegistry(state).importToClassMap.put(ImportKey, (compilation, _import) => {
      val elements = getParts(_import)
      val packageParts = elements.dropRight(1)
      val importedClassName = elements.last

      val qualifiedClassName = QualifiedClassName(packageParts ++ Seq(importedClassName))
      val result = Seq((importedClassName, qualifiedClassName)).toMap
      result
    })
    super.inject(state)
  }

  override def dependencies: Set[Contract] = Set(JavaClassSkeleton)

  override def description: String = "Allows importing a single class using an import statement."
}
