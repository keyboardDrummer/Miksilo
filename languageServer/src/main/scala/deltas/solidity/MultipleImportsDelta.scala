package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeShape}

object MultipleImportsDelta extends DeltaWithGrammar {

  object Shape extends NodeShape
  object FileName extends NodeField
  object Imports extends NodeField

  object ElementShape extends NodeShape
  object Name extends NodeField
  object NewName extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    val _grammars = grammars
    import grammars._

    val newName = find(FileImportDelta.NewName)
    val namedImport = identifier.as(Name) ~~ newName asNode ElementShape
    val members = "{" ~~ namedImport.manySeparated("," ~ printSpace).as(Imports) ~~ "}"
    val multipleImports = SingleImportDelta.importPattern(_grammars, Shape, members)
    find(SolidityFile.Members).addAlternative(multipleImports)
  }

  override def description = "Adds importing an entire file"

  override def dependencies = Set(SolidityFile)
}
