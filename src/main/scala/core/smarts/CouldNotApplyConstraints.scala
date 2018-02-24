package core.smarts

case class CouldNotApplyConstraints(constraints: Seq[Constraint]) extends SolveException {
  override def toString: String = s"Left with constraints: $constraints"
}
