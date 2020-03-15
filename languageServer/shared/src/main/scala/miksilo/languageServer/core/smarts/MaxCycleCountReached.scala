package miksilo.languageServer.core.smarts

case class MaxCycleCountReached(max: Int) extends SolveException {
  override def toString: String = "Max cycle count reached"
}
