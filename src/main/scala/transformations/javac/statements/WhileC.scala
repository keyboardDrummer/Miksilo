package transformations.javac.statements

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node._
import core.particles.path.{Path, SequenceElement}
import transformations.bytecode.ByteCodeMethodInfo
import transformations.bytecode.additions.LabelledLocations
import transformations.bytecode.simpleBytecode.InferredStackFrames
import transformations.javac.expressions.ExpressionSkeleton

object WhileC extends StatementInstance with WithLanguageRegistry {

  override val key = WhileKey

  override def toByteCode(_while: Path, compilation: Compilation): Seq[Node] = {
    val methodInfo = _while.findAncestorClass(ByteCodeMethodInfo.MethodInfoKey)
    val startLabel = LabelledLocations.getUniqueLabel("start", methodInfo, compilation)
    val endLabel = LabelledLocations.getUniqueLabel("end", methodInfo, compilation)

    val conditionInstructions = ExpressionSkeleton.getToInstructions(compilation)(getCondition(_while))
    getRegistry(compilation).whileStartLabels += _while.current -> startLabel

    val body = getBody(_while)
    val bodyInstructions = body.flatMap(statement => StatementSkeleton.getToInstructions(compilation)(statement))

    Seq(InferredStackFrames.label(startLabel)) ++
      conditionInstructions ++
      Seq(LabelledLocations.ifZero(endLabel)) ++
      bodyInstructions ++ Seq(LabelledLocations.goTo(startLabel), InferredStackFrames.label(endLabel))
  }

  def getCondition[T <: NodeLike](_while: T) = _while(Condition).asInstanceOf[T]

  def getBody[T <: NodeLike](_while: T) = _while(Body).asInstanceOf[Seq[T]]

  override def dependencies: Set[Contract] = super.dependencies ++ Set(BlockC)

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    import grammars._
    val statementGrammar = find(StatementSkeleton.StatementGrammar)
    val expression = find(ExpressionSkeleton.ExpressionGrammar)
    val blockGrammar = find(BlockC.BlockGrammar)
    val whileGrammar =
      "while" ~> expression.inParenthesis.as(Condition) %
      blockGrammar.as(Body) asNode WhileKey
    statementGrammar.addOption(whileGrammar)
  }

  def create(condition: Node, body: Seq[Node]) = new Node(WhileKey, Condition -> condition, Body -> body)

  object WhileKey extends NodeClass

  object Condition extends NodeField

  object Body extends NodeField

  override def description: String = "Enables using the while construct."

  def startKey(_while: NodeLike) = (_while,"start")
  def endKey(_while: NodeLike) = (_while,"end")
  override def getNextStatements(obj: Path, labels: Map[Any, Path]): Set[Path] = {
    super.getNextStatements(obj, labels) ++ getBody(obj).take(1)
  }

  override def getLabels(_whilePath: Path): Map[Any, Path] = {
    val _while = _whilePath.asInstanceOf[SequenceElement]
    val current = _while.current
    val next = _while.next
    var result: Map[Any,Path] = Map(startKey(current) -> _while, endKey(current) -> next)
    val body = getBody(_whilePath)
    if (body.nonEmpty)
      result += getNextLabel(body.last) -> next
    result
  }

  class Registry {
    var whileStartLabels: Map[Node, String] = Map.empty
  }

  override def createRegistry = new Registry()
}
