package transformations.javac.statements

import core.particles.Language
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeClass, NodeField}
import core.particles.path.Path
import transformations.bytecode.simpleBytecode.InferredStackFrames

object JustJavaLabel extends StatementInstance {
  override val key = LabelKey

  object LabelKey extends NodeClass
  object Name extends NodeField

  def label(name: String) = new Node(LabelKey, Name -> name)

  override def toByteCode(statement: Path, state: Language): Seq[Node] = {
    Seq(InferredStackFrames.label(getName(statement.current)))
  }

  def getName(statement: Node) = statement(Name).asInstanceOf[String]

  object JavaLabelGrammar

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val statementGrammar = grammars.find(StatementSkeleton.StatementGrammar)
    statementGrammar.addOption(grammars.create(JavaLabelGrammar, ("label" ~~> identifier <~ ";").asNode(LabelKey, Name)))
  }

  override def description: String = "Adds a label statement"

  override def getLabels(obj: Path): Map[Any, Path] = {
    super.getLabels(obj) + (getName(obj.current) -> obj)
  }
}
