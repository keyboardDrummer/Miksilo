package core.bigrammar.printer

import core.document.Empty
import core.responsiveDocument.ResponsiveDocument

case class RootError(depth: Int, partial: ResponsiveDocument, inner: Any) extends PrintError {

   def toDocument: ResponsiveDocument = ("Root:": ResponsiveDocument) %
     (s"Inner Exception: $inner": ResponsiveDocument) %
     (s"depth = $depth": ResponsiveDocument) %
     "Partial: " % partial.indent(4) %
     // % s"trace = " % inner.getStackTrace.map(e => e.toString: ResponsiveDocument).reduce((a, b) => a % b).indent(4)
     Empty

   override def mapPartial(f: (ResponsiveDocument) => ResponsiveDocument): PrintError = RootError(1 + depth, f(partial), inner)
 }
