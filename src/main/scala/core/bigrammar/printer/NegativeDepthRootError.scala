package core.bigrammar.printer

import core.document.Empty
import core.responsiveDocument.ResponsiveDocument

case class NegativeDepthRootError(message: Any, depth: Int) extends PrintError {

  def toDocument: ResponsiveDocument = message.toString

  override def partial = Empty
}
