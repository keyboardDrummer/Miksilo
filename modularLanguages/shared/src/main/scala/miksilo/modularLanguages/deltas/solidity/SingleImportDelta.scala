package miksilo.modularLanguages.deltas.solidity

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.bigrammar.grammars.StringLiteralGrammar
import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.core.node.{NodeField, NodeShape}

object SingleImportDelta extends DeltaWithGrammar {

  object Shape extends NodeShape
  object Wildcard
  object ElementShape extends NodeShape
  object ElementName extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    val _grammars = grammars
    import grammars._
    val newName = find(FileImportDelta.NewName)
    val wildCard = "*" ~> value(Wildcard)
    val singleImport = importPattern(_grammars, Shape, (identifier.as(ElementName) | wildCard) ~~ newName)
    find(FileWithMembersDelta.Members).addAlternative(singleImport)
  }

  def importPattern(grammars: LanguageGrammars, shape: NodeShape, inner: BiGrammar): BiGrammar = {
    import grammars._

    "import" ~~ inner ~~ "from" ~~ StringLiteralGrammar.as(FileImportDelta.FileName) asNode Shape
  }

  override def description = "Adds importing an entire file"

  override def dependencies = Set(FileWithMembersDelta)
}
