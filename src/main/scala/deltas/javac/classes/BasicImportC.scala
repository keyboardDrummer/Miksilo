package deltas.javac.classes

import core.particles.grammars.LanguageGrammars
import core.particles.node.{GrammarKey, Node, NodeClass, NodeField}
import core.particles.{Contract, DeltaWithGrammar, Language}
import deltas.javac.classes.skeleton.{JavaClassSkeleton, QualifiedClassName}

object BasicImportC extends DeltaWithGrammar {

  object ImportKey extends NodeClass
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
