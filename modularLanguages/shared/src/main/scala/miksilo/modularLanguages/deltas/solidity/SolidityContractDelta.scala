package miksilo.modularLanguages.deltas.solidity

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.editorParser.document.BlankLine
import miksilo.modularLanguages.core.node.{NodeField, NodeLike, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.DeclarationHasType
import miksilo.languageServer.core.smarts.types.objects.{Type, TypeFromDeclaration}
import miksilo.modularLanguages.deltas.ConstraintSkeleton
import miksilo.modularLanguages.deltas.HasNameDelta.{HasName, Name}
import miksilo.modularLanguages.deltas.bytecode.types.{TypeSkeleton, UnqualifiedObjectTypeDelta}
import miksilo.modularLanguages.deltas.classes.ClassDelta
import miksilo.modularLanguages.deltas.classes.ClassDelta.ClassImports
import miksilo.modularLanguages.deltas.expression.ExpressionDelta
import miksilo.modularLanguages.deltas.javac.classes.skeleton.HasConstraintsDelta

object SolidityContractDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object ContractType extends NodeField
  object SuperContracts extends NodeField

  object SuperShape extends NodeShape
  object SuperName extends NodeField
  object SuperArguments extends NodeField

  implicit class ContractLike[T <: NodeLike](val node: T) extends HasName[T] {
    def imports = node(ClassImports).asInstanceOf[Seq[T]]
    def imports_=(value: Seq[T]): Unit = node(ClassImports) = value

    def members = node(ClassDelta.Members).asInstanceOf[Seq[T]]
    def members_=(value: Seq[T]): Unit = node(ClassDelta.Members) = value

    def parents: Seq[T] = node(SuperContracts).asInstanceOf[Seq[T]]
    def parents_=(value: Seq[T]): Unit = node(SuperContracts) = value

    def contractType: String = node.getValue(ContractType).asInstanceOf[String]
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val contractType = ("contract" | "interface" | "library").as(ContractType)
    val objectType = find(UnqualifiedObjectTypeDelta.AnyObjectTypeGrammar)
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val inheritanceSpecifier: BiGrammar = objectType.as(SuperName) ~
      (expression.someSeparated("," ~ printSpace).inParenthesis | value(Seq.empty)).as(SuperArguments) asNode SuperShape
    val inheritance = (printSpace ~ "is" ~~ inheritanceSpecifier.someSeparated("," ~ printSpace) | value(Seq.empty)).as(SuperContracts)
    val member = create(ClassDelta.Members)
    val members = member.manySeparatedVertical(BlankLine).as(ClassDelta.Members)
    val contract = contractType ~~ identifier.as(Name) ~ inheritance ~ "{" % members % "}" asNode ClassDelta.Shape
    find(FileWithMembersDelta.Members).addAlternative(contract)
  }

  override def description = "Adds the contract/interface/library"

  override def dependencies = Set(FileWithMembersDelta, UnqualifiedObjectTypeDelta)

  override def shape = ClassDelta.Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val contractLike: ContractLike[NodePath] = path
    val contractDeclaration = builder.declare(contractLike.name, parentScope, path.getField(Name))
    val contractStaticType = TypeFromDeclaration(contractDeclaration)
    val contractInstanceType = TypeFromDeclaration(contractDeclaration) // TODO fix this
    builder.add(DeclarationHasType(contractDeclaration, contractStaticType))
    builder.assignSubType(TypeSkeleton.typeKind, contractStaticType)
    val contractScope = builder.declareScope(contractDeclaration, parentScope, s"contract '${contractLike.name}'")

    if (Set("interface", "contract").contains(contractLike.contractType)) {
      val addressDeclaration = SolidityLibraryDelta.addressDeclaration(compilation)
      val fromAddressMethod = SolidityFunctionTypeDelta.createType(
        Seq[Type](TypeFromDeclaration(addressDeclaration)),
        Seq[Type](contractInstanceType))
      builder.declare(contractLike.name, parentScope, _type = Some(fromAddressMethod))
    }

    for(member <- contractLike.members) {
      ConstraintSkeleton.constraints(compilation, builder, member, contractScope)
    }
  }
}



