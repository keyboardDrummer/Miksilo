package core.grammar

import core.exceptions.BadInputException

case class ParseException(message: String) extends BadInputException {
  override def toString = message
}
