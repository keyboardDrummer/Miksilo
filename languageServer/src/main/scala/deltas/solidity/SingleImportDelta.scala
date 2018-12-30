package deltas.solidity

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.StringLiteral
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeShape}

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
    find(SolidityFile.Members).addAlternative(singleImport)
  }

  def importPattern(grammars: LanguageGrammars, shape: NodeShape, inner: BiGrammar): BiGrammar = {
    import grammars._

    "import" ~~ inner ~~ "from" ~~ StringLiteral.as(FileImportDelta.FileName) asNode Shape
  }

  override def description = "Adds importing an entire file"

  override def dependencies = Set(SolidityFile)
}
