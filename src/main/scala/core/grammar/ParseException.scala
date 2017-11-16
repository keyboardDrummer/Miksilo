package core.grammar

import core.deltas.exceptions.BadInputException

case class ParseException(message: String) extends BadInputException {
  override def toString = message
}
