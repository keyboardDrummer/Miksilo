package miksilo.languageServer.core.smarts

case class CouldNotApplyConstraints(constraints: Seq[Constraint]) extends SolveException {
  override def toString: String = s"Left with constraints:\n${constraints.mkString("\n")}"
}
