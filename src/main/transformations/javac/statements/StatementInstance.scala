package transformations.javac.statements

import core.particles._

trait StatementInstance extends ParticleWithGrammar {

  override def inject(state: CompilationState): Unit = {
    StatementSkeleton.getState(state).instances.put(key, this)
    super.inject(state)
  }

  val key: AnyRef

  def toByteCode(statement: MetaObjectWithOrigin, state: CompilationState): Seq[MetaObject]

  override def dependencies: Set[Contract] = Set(StatementSkeleton) ++ super.dependencies

  case class SequenceDoesNotEndInJump(sequence: Seq[MetaObject]) extends Exception
  {
    override def toString = s"SequenceDoesNotEndInJump: $sequence"
  }

  def getNextLabel(statement: MetaObjectWithOrigin) = (statement,"next")
  def getNextStatements(obj: MetaObjectWithOrigin, labels: Map[Any, MetaObjectWithOrigin]): Set[MetaObjectWithOrigin] = {
    val selection = obj.origin.asInstanceOf[SequenceSelection]
    if (selection.hasNext)
      return Set(selection.next)

    val nextOption = labels.get(getNextLabel(obj))
    if (nextOption.nonEmpty)
      return Set(nextOption.get)

    throw SequenceDoesNotEndInJump(selection.parent.obj(selection.field).asInstanceOf[Seq[MetaObject]])
  }

  def getLabels(obj: MetaObjectWithOrigin): Map[Any, MetaObjectWithOrigin] = Map.empty

  def definedVariables(state: CompilationState, obj: MetaObject): Map[String, MetaObject] = Map.empty
}