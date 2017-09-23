package transformations.javac.methods

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import core.particles.path.Path
import transformations.bytecode.coreInstructions.VoidReturnInstructionDelta$
import transformations.javac.statements.{StatementInstance, StatementSkeleton}

object ReturnVoidC extends StatementInstance {

  override def dependencies: Set[Contract] = Set(MethodC, VoidReturnInstructionDelta$)

  override def getNextStatements(obj: Path, labels: Map[Any, Path]): Set[Path] = Set.empty

  def returnToLines(_return: Node, compiler: MethodCompiler): Seq[Node] = {
    Seq(VoidReturnInstructionDelta$.voidReturn)
  }

  override def transformGrammars(grammars: GrammarCatalogue, state: CompilationState): Unit = {
    val statement = grammars.find(StatementSkeleton.StatementGrammar)

    val returnExpression = ("return" ~ ";") ~> produce(_return)
    statement.inner = statement.inner | returnExpression
  }

  def _return: Node = new Node(ReturnVoidKey)

  object ReturnVoidKey extends Key

  override val key: Key = ReturnVoidKey

  override def toByteCode(_return: Path, state: CompilationState): Seq[Node] = {
    Seq(VoidReturnInstructionDelta$.voidReturn)
  }

  override def description: String = "Allows returning void."
}
