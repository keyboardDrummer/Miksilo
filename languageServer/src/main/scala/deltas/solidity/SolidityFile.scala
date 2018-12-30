package deltas.solidity

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.{BodyGrammar, LanguageGrammars}
import core.document.BlankLine
import core.language.Language
import core.language.node.{NodeField, NodeShape}

object SolidityFile extends DeltaWithGrammar {
  object Shape extends NodeShape
  object Members extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val member: BiGrammar = create(Members)
    find(BodyGrammar).inner = member.manySeparatedVertical(BlankLine).as(Members).asNode(Shape)
  }

  override def description = "Defines the solidity file"

  override def dependencies = Set.empty
}






