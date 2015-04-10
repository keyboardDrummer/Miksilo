package core.bigrammar.printer

import core.bigrammar.BiGrammar
import core.responsiveDocument.ResponsiveDocument

case class NestedError(value: Any, grammar: BiGrammar, inner: PrintError) extends PrintError
 {
   def partial = inner.partial
   def mapPartial(f: ResponsiveDocument => ResponsiveDocument) = NestedError(value, grammar, inner.mapPartial(f))
   val depth = inner.depth

   override def toDocument = ("Nested:": ResponsiveDocument) % (
     ("Value:": ResponsiveDocument) ~~ value.toString %
     ("Grammar:": ResponsiveDocument) ~~ grammar.toString %
     inner.toDocument)
 }
