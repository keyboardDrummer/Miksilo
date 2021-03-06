package miksilo.modularLanguages.core.bigrammar.printer

import miksilo.editorParser.responsiveDocument.ResponsiveDocument

trait PrintError extends Throwable
 {
   def toDocumentWithPartial = toDocument % s"Depth: $depth" % "Partial:" % partial.indent(4)

   override def toString = toDocumentWithPartial.renderString()
   def toDocument: ResponsiveDocument
   def partial: ResponsiveDocument
   val depth: Int
 }
