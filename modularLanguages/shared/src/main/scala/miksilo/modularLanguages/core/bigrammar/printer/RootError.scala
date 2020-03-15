package miksilo.modularLanguages.core.bigrammar.printer

import miksilo.editorParser.document.Empty

case class RootError(message: Any) extends PrintError {
  override def toDocument = message.toString

  override def partial = Empty

  override val depth = 0
}
