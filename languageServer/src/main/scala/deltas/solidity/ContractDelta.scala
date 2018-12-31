package deltas.solidity

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.document.BlankLine
import core.language.Language
import core.language.node.{NodeField, NodeShape}
import deltas.expressions.ExpressionDelta

object ContractDelta extends DeltaWithGrammar {

  object Shape extends NodeShape
  object ContractType extends NodeField
  object SuperContracts extends NodeField
  object Members extends NodeField

  object SuperShape extends NodeShape
  object SuperName extends NodeField
  object SuperArguments extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val contractType = ("contract" | "interface" | "library").as(ContractType)
    val objectType = find(ObjectTypeDelta.Shape)
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val inheritanceSpecifier: BiGrammar = objectType.as(SuperName) ~
      (expression.someSeparated("," ~ printSpace).inParenthesis | value(Seq.empty)).as(SuperArguments) asNode SuperShape
    val inheritance = (printSpace ~ "is" ~~ inheritanceSpecifier.someSeparated("," ~ printSpace) | value(Seq.empty)).as(SuperContracts)
    val member = create(Members)
    val members = member.manySeparatedVertical(BlankLine).as(Members)
    val contract = contractType ~~ identifier ~ inheritance ~ "{" % members % "}"
    find(SolidityFile.Members).addAlternative(contract)
  }

  override def description = "Adds the contract/interface/library"

  override def dependencies = Set(SolidityFile, ObjectTypeDelta)
}



