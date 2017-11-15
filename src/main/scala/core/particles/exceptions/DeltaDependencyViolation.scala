package core.particles.exceptions

import core.particles.Contract

case class DeltaDependencyViolation(dependency: Contract, dependent: Contract) extends CompilerException {
  override def toString = s"dependency '${dependency.name}' from '${dependent.name}' is not satisfied"
}
