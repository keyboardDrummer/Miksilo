package core.grammar

import core.particles.exceptions.BadInputException

case class ParseException(message: String) extends BadInputException {
  override def toString = message
}
