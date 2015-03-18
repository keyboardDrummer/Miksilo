package transformations.javac.statements

import core.particles._
import core.particles.path.{Path, SequenceSelection}

trait StatementInstance extends ParticleWithGrammar {

  override def inject(state: CompilationState): Unit = {
    StatementSkeleton.getState(state).instances.put(key, this)
    super.inject(state)
  }

  val key: AnyRef

  def toByteCode(statement: Path, state: CompilationState): Seq[MetaObject]

  override def dependencies: Set[Contract] = Set(StatementSkeleton) ++ super.dependencies

  case class SequenceDoesNotEndInJump(sequence: Seq[MetaObject]) extends Exception
  {
    override def toString = s"SequenceDoesNotEndInJump: $sequence"
  }

  def getNextLabel(statement: Path) = (statement,"next")
  def getNextStatements(obj: Path, labels: Map[Any, Path]): Set[Path] = {
    val selection = obj.asInstanceOf[SequenceSelection]
    if (selection.hasNext)
      return Set(selection.next)

    val nextOption = labels.get(getNextLabel(obj))
    if (nextOption.nonEmpty)
      return Set(nextOption.get)

    throw SequenceDoesNotEndInJump(selection.parent.current(selection.field).asInstanceOf[Seq[MetaObject]])
  }

  def getLabels(obj: Path): Map[Any, Path] = Map.empty

  def definedVariables(state: CompilationState, obj: MetaObject): Map[String, MetaObject] = Map.empty
}