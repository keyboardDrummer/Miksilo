package transformations.javac.methods

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import core.particles.path.Path
import transformations.bytecode.coreInstructions.VoidReturnInstructionC
import transformations.javac.statements.{StatementInstance, StatementSkeleton}

object ReturnVoidC extends StatementInstance {

  override def dependencies: Set[Contract] = Set(MethodC, VoidReturnInstructionC)

  override def getNextStatements(obj: Path, labels: Map[Any, Path]): Set[Path] = Set.empty

  def returnToLines(_return: Node, compiler: MethodCompiler): Seq[Node] = {
    Seq(VoidReturnInstructionC.voidReturn)
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val statement = grammars.find(StatementSkeleton.StatementGrammar)

    val returnExpression = ("return" ~ ";") ~> produce(_return)
    statement.inner = statement.inner | returnExpression
  }

  def _return: Node = new Node(ReturnVoidKey)

  object ReturnVoidKey

  override val key: AnyRef = ReturnVoidKey

  override def toByteCode(_return: Path, state: CompilationState): Seq[Node] = {
    Seq(VoidReturnInstructionC.voidReturn)
  }

  override def description: String = "Allows returning void."
}
