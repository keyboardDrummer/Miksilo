package transformations.javac.statements

import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeLike}
import core.particles.path.Path
import core.particles.{CompilationState, ParticleWithGrammar}
import transformations.bytecode.additions.LabelledLocations
import transformations.bytecode.simpleBytecode.InferredStackFrames

object JavaGotoC extends ParticleWithGrammar {

  override def inject(state: CompilationState): Unit = {
    GotoInner.inject(state)
    LabelInner.inject(state)
    super.inject(state)
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    GotoInner.transformGrammars(grammars)
    LabelInner.transformGrammars(grammars)
  }

  object LabelInner extends StatementInstance {
    override val key: Key = LabelKey

    object LabelKey extends Key
    object Name extends Key

    override def toByteCode(statement: Path, state: CompilationState): Seq[Node] = {
      Seq(InferredStackFrames.label(getName(statement.current)))
    }

    def getName(statement: Node) = statement(Name).asInstanceOf[String]

    override def transformGrammars(grammars: GrammarCatalogue): Unit = {
      val statementGrammar = grammars.find(StatementSkeleton.StatementGrammar)
      statementGrammar.addOption(("label" ~~> identifier <~ ";").asNode(LabelKey, Name))
    }

    override def description: String = "Adds a label statement"

    override def getLabels(obj: Path): Map[Any, Path] = {
      super.getLabels(obj) + (getName(obj.current) -> obj)
    }
  }

  object GotoInner extends StatementInstance {
    override val key: Key = GotoKey

    object GotoKey extends Key
    object Target extends Key

    override def toByteCode(statement: Path, state: CompilationState): Seq[Node] = {
      Seq(LabelledLocations.goTo(getTarget(statement.current)))
    }

    def getTarget(node: Node) = node(Target).asInstanceOf[String]

    override def transformGrammars(grammars: GrammarCatalogue): Unit = {
      val statementGrammar = grammars.find(StatementSkeleton.StatementGrammar)
      statementGrammar.addOption(("goto" ~~> identifier <~ ";").asNode(GotoKey, Target))
    }

    override def getNextStatements(obj: Path, labels: Map[Any, Path]): Set[Path] = Set(labels(getTarget(obj.current)))

    override def getNextLabel(statement: Path): (Path, String) = super.getNextLabel(statement)

    override def description: String = "Adds a go to statement"
  }

  override def description: String = "Adds goto and label statements"
}
