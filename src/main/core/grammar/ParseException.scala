package core.grammar

import core.transformation.BadInputException

case class ParseException(message: String) extends BadInputException {
  override def toString = message
}
