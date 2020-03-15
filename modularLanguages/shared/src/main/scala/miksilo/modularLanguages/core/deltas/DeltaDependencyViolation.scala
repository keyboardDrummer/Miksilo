package miksilo.modularLanguages.core.deltas

case class DeltaDependencyViolation(dependency: Contract, dependent: Contract) extends Exception {
  override def toString = s"dependency '${dependency.name}' from '${dependent.name}' is not satisfied"
}
