package deltas.solidity

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.document.BlankLine
import core.language.Language
import core.language.node.{NodeField, NodeShape}
import deltas.statement.BlockDelta

object ContractDelta extends DeltaWithGrammar {

  object Shape extends NodeShape
  object ContractType extends NodeField
  object SuperContracts extends NodeField
  object Members extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val contractType = ("contract" | "interface" | "library").as(ContractType)
    //val inheritanceSpecifier: BiGrammar = ??? //InheritanceSpecifier = UserDefinedTypeName ( '(' Expression ( ',' Expression )* ')' )?
    val inheritance =
      //printSpace ~ "is" ~~ inheritanceSpecifier.someSeparated("," ~ printSpace).as(SuperContracts) |
      value(Seq.empty).as(SuperContracts)
    val member = create(Members)
    val members = member.manySeparatedVertical(BlankLine).as(Members)
    val contract = contractType ~~ identifier ~ inheritance ~ "{" % members % "}"
    find(SolidityFile.Members).addAlternative(contract)
  }

  override def description = "Adds the contract/interface/library"

  override def dependencies = Set(SolidityFile)
}

object SolidityFunctionDelta extends DeltaWithGrammar {

  object Shape extends NodeShape
  object Name extends NodeField
  object Parameters extends NodeField
  object ReturnValues extends NodeField
  object Body extends NodeField
  object Modifiers extends NodeField

  object ParameterShape extends NodeShape // TODO try to re-use methodParameters.
  object ParameterName extends NodeField
  object ParameterType extends NodeField
  object ParameterStorageLocation extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val typeGrammar = find(SolidityTypeDelta.Shape)
    val storageLocation: BiGrammar = "memory" | "storage" | "calldata"
    val parameter = typeGrammar.as(ParameterType) ~
      (printSpace ~ storageLocation).option.as(ParameterStorageLocation) ~
      (printSpace ~ identifier).option.as(ParameterName) asNode ParameterShape
    val parameterList = "(" ~> parameter.manySeparated("," ~ printSpace) ~< ")"
    val name = identifier.option.as(Name)
    val stateMutability = "pure" | "view" | "payable"
    val modifiers = (printSpace ~ (stateMutability | "external" | "public" | "internal" | "private")).many.as(Modifiers)
    val returnValues = printSpace ~ "returns" ~~ parameterList.as(ReturnValues) | value(Seq.empty).as(ReturnValues)
    val blockGrammar: BiGrammar = find(BlockDelta.Grammar)
    val body = ";" ~> value(Seq.empty).as(Body) | blockGrammar.as(Body)
    val grammar = "function" ~~ name ~ parameterList.as(Parameters) ~ modifiers ~ returnValues ~~ body
    find(ContractDelta.Members).addAlternative(grammar)
  }

  override def description = "Adds solidity functions"

  override def dependencies = Set(SolidityTypeDelta, BlockDelta)
}

