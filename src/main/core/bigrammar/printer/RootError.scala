package core.bigrammar.printer

import core.bigrammar.BiGrammar
import core.document.Empty
import core.responsiveDocument.ResponsiveDocument

case class RootError(depth: Int, partial: ResponsiveDocument, value: Any, grammar: BiGrammar, inner: Throwable) extends PrintError {

   def toDocument: ResponsiveDocument = ("Root:": ResponsiveDocument) %
     (s"Inner Exception: $inner": ResponsiveDocument) %
     s"Value: $value : ${value.getClass}" %
     s"Grammar = $grammar" %
     (s"depth = $depth": ResponsiveDocument) %
     "Partial: " % partial.indent(4) %
     // % s"trace = " % inner.getStackTrace.map(e => e.toString: ResponsiveDocument).reduce((a, b) => a % b).indent(4)
     Empty

   override def mapPartial(f: (ResponsiveDocument) => ResponsiveDocument): PrintError = new RootError(1 + depth, f(partial), value, grammar, inner)
 }
