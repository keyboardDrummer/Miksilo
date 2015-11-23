package transformations.javac.statements

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeLike}
import core.particles.path.{Path, SequenceSelection}
import transformations.bytecode.additions.LabelledLocations
import transformations.bytecode.simpleBytecode.InferredStackFrames
import transformations.javac.expressions.ExpressionSkeleton

object WhileC extends StatementInstance with WithState {

  override val key: Key = WhileKey

  override def toByteCode(_while: Path, state: CompilationState): Seq[Node] = {
    val startLabel = state.getUniqueLabel("start")
    val endLabel = state.getUniqueLabel("end")

    val conditionInstructions = ExpressionSkeleton.getToInstructions(state)(getCondition(_while))
    getState(state).whileStartLabels += _while.current -> startLabel

    val body = getBody(_while)
    val bodyInstructions = body.flatMap(statement => StatementSkeleton.getToInstructions(state)(statement))

    Seq(InferredStackFrames.label(startLabel)) ++
      conditionInstructions ++
      Seq(LabelledLocations.ifZero(endLabel)) ++
      bodyInstructions ++ Seq(LabelledLocations.goTo(startLabel), InferredStackFrames.label(endLabel))
  }

  def getCondition[T <: NodeLike](_while: T) = _while(Condition).asInstanceOf[T]

  def getBody[T <: NodeLike](_while: T) = _while(Body).asInstanceOf[Seq[T]]

  override def dependencies: Set[Contract] = super.dependencies ++ Set(BlockC)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val statementGrammar = grammars.find(StatementSkeleton.StatementGrammar)
    val expression = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val blockGrammar = grammars.find(BlockC.BlockGrammar)
    val whileInner =
      "while" ~> expression.inParenthesis % blockGrammar
    val whileGrammar =
      whileInner.asNode(WhileKey, Condition, Body)
    statementGrammar.addOption(whileGrammar)
  }

  def _while(condition: Node, body: Seq[Node]) = new Node(WhileKey, Condition -> condition, Body -> body)

  object WhileKey extends core.particles.node.Key

  object Condition extends Key

  object Body extends Key

  override def description: String = "Enables using the while construct."

  def startKey(_while: NodeLike) = (_while,"start")
  def endKey(_while: NodeLike) = (_while,"end")
  override def getNextStatements(obj: Path, labels: Map[Any, Path]): Set[Path] = {
    super.getNextStatements(obj, labels) ++ getBody(obj).take(1)
  }

  override def getLabels(_whilePath: Path): Map[Any, Path] = {
    val _while = _whilePath.asInstanceOf[SequenceSelection]
    val current = _while.current
    val next = _while.next
    var result: Map[Any,Path] = Map(startKey(current) -> _while, endKey(current) -> next)
    val body = getBody(_whilePath)
    if (body.nonEmpty)
      result += getNextLabel(body.last) -> next
    result
  }

  class State {
    var whileStartLabels: Map[Node, String] = Map.empty
  }

  override def createState = new State()
}
