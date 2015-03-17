package transformations.javac.statements


import core.particles.grammars.GrammarCatalogue
import core.particles._
import transformations.bytecode.additions.LabelledTargets
import transformations.bytecode.simpleBytecode.InferredStackFrames
import transformations.javac.expressions.ExpressionSkeleton

object WhileC extends StatementInstance with WithState {

  override val key: AnyRef = WhileKey

  override def toByteCode(_while: MetaObjectWithOrigin, state: CompilationState): Seq[MetaObject] = {
    val startLabel = state.getUniqueLabel("start")
    val endLabel = state.getUniqueLabel("end")

    val conditionInstructions = ExpressionSkeleton.getToInstructions(state)(getCondition(_while))
    getState(state).whileStartLabels += _while.obj -> startLabel

    val body = getBody(_while)
    val bodyInstructions = body.flatMap(statement => StatementSkeleton.getToInstructions(state)(statement))

    Seq(InferredStackFrames.label(startLabel)) ++
      conditionInstructions ++
      Seq(LabelledTargets.ifZero(endLabel)) ++
      bodyInstructions ++ Seq(LabelledTargets.goTo(startLabel), InferredStackFrames.label(endLabel))
  }

  def getCondition[T <: MetaLike](_while: T) = _while(WhileCondition).asInstanceOf[T]

  def getBody[T <: MetaLike](_while: T) = _while(WhileBody).asInstanceOf[Seq[T]]

  override def dependencies: Set[Contract] = super.dependencies ++ Set(BlockC)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val statementGrammar = grammars.find(StatementSkeleton.StatementGrammar)
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val blockGrammar = grammars.find(BlockC.BlockGrammar)
    val whileGrammar = "while" ~> ("(" ~> expressionGrammar <~ ")") % blockGrammar ^^
      parseMap(WhileKey, WhileCondition, WhileBody)
    statementGrammar.addOption(whileGrammar)
  }

  def _while(condition: MetaObject, body: Seq[MetaObject]) = new MetaObject(WhileKey, WhileCondition -> condition, WhileBody -> body)

  object WhileKey

  object WhileCondition

  object WhileBody

  override def description: String = "Enables using the while construct."

  def startKey(_while: MetaLike) = (_while,"start")
  def endKey(_while: MetaLike) = (_while,"end")
  override def getNextStatements(obj: MetaObjectWithOrigin, labels: Map[Any, MetaObjectWithOrigin]): Set[MetaObjectWithOrigin] = {
    super.getNextStatements(obj, labels) ++ getBody(obj).take(1)
  }

  override def getLabels(_while: MetaObjectWithOrigin): Map[Any, MetaObjectWithOrigin] = {
    val sequenceOrigin = _while.origin.asInstanceOf[SequenceSelection]
    val current = sequenceOrigin.current
    val next = sequenceOrigin.next
    var result: Map[Any,MetaObjectWithOrigin] = Map(startKey(current) -> current, endKey(current) -> next)
    val body = getBody(_while)
    if (body.nonEmpty)
      result += getNextLabel(body.last) -> next
    result
  }

  class State {
    var whileStartLabels: Map[MetaObject, String] = Map.empty
  }

  override def createState = new State()
}
