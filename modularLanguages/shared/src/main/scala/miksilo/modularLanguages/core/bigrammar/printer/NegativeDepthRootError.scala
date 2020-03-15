package miksilo.modularLanguages.core.bigrammar.printer

import miksilo.editorParser.document.Empty
import miksilo.editorParser.responsiveDocument.ResponsiveDocument

case class NegativeDepthRootError(message: Any, depth: Int) extends PrintError {

  def toDocument: ResponsiveDocument = message.toString

  override def partial = Empty
}
