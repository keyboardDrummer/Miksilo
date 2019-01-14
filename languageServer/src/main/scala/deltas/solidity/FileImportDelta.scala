package deltas.solidity

import core.bigrammar.grammars.StringLiteral
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeShape}

object FileImportDelta extends DeltaWithGrammar {

  object Shape extends NodeShape
  object FileName extends NodeField
  object NewName extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val newName = create(NewName, (printSpace ~> "as" ~~> identifier).option.as(NewName))
    val simpleImport = "import" ~~ StringLiteral.as(FileName) ~~ (printSpace ~> "as" ~~> identifier).option.as(NewName) ~ ";" asNode Shape
    find(FileWithMembersDelta.Members).addAlternative(simpleImport)
  }

  override def description = "Adds importing an entire file"

  override def dependencies = Set(FileWithMembersDelta)
}