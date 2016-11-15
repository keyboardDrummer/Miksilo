package transformations.javac.classes

import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import core.particles.{CompilationState, Contract, DeltaWithGrammar}
import transformations.javac.classes.skeleton.{QualifiedClassName, JavaClassSkeleton}

object BasicImportC extends DeltaWithGrammar {

  object ImportKey
  object ElementsKey

  object ImportPathGrammar

  def _import(elements: Seq[String]) = new Node(ImportKey, ElementsKey -> elements)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val importPath = grammars.create(ImportPathGrammar, identifier.someSeparated(".") ^^ parseMap(ImportKey, ElementsKey))
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
