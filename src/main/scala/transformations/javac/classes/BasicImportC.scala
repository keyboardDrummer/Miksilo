package transformations.javac.classes

import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import core.particles.{CompilationState, Contract, DeltaWithGrammar}
import transformations.javac.classes.skeleton.{JavaClassSkeleton, QualifiedClassName}

object BasicImportC extends DeltaWithGrammar {

  object ImportKey extends Key
  object ElementsKey extends Key

  object ImportPathGrammar

  def _import(elements: Seq[String]) = new Node(ImportKey, ElementsKey -> elements)

  override def transformGrammars(grammars: GrammarCatalogue, state: CompilationState): Unit = {
    val importPath = grammars.create(ImportPathGrammar, identifier.someSeparated(".").asNode(ImportKey, ElementsKey))
    val basicImport = "import" ~~> importPath <~ ";"
    grammars.find(JavaClassSkeleton.ImportGrammar).addOption(basicImport)
  }

  def getParts(_import: Node) = _import(ElementsKey).asInstanceOf[Seq[String]]

  override def inject(state: CompilationState): Unit = {
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
