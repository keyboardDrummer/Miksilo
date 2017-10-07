package transformations.javac.classes

import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField}
import core.particles.{Contract, DeltaWithGrammar, Language}
import transformations.javac.classes.skeleton.{JavaClassSkeleton, QualifiedClassName}

object BasicImportC extends DeltaWithGrammar {

  object ImportKey extends NodeClass
  object ElementsKey extends NodeField

  object ImportPathGrammar

  def _import(elements: Seq[String]) = new Node(ImportKey, ElementsKey -> elements)

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val importPath = grammars.create(ImportPathGrammar, identifier.someSeparated(".").as(ElementsKey).asNode(ImportKey))
    val basicImport = "import" ~~> importPath ~< ";"
    grammars.find(JavaClassSkeleton.ImportGrammar).addOption(basicImport)
  }

  def getParts(_import: Node) = _import(ElementsKey).asInstanceOf[Seq[String]]

  override def inject(state: Language): Unit = {
    JavaClassSkeleton.getState(state).importToClassMap.put(ImportKey, _import => {
      val elements = getParts(_import)
      val packageParts = elements.dropRight(1)
      val importedClassName = elements.last

      val qualifiedClassName = new QualifiedClassName(packageParts ++ Seq(importedClassName))
      val result = Seq((importedClassName, qualifiedClassName)).toMap
      result
    })
    super.inject(state)
  }

  override def dependencies: Set[Contract] = Set(JavaClassSkeleton)

  override def description: String = "Allows importing a single class using an import statement."
}
