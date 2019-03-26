package deltas.solidity

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.document.BlankLine
import core.language.node.{NodeField, NodeLike, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.DeclarationHasType
import core.smarts.types.objects.{Type, TypeFromDeclaration}
import deltas.ConstraintSkeleton
import deltas.HasNameDelta.{HasName, Name}
import deltas.javac.classes.skeleton.JavaClassDelta
import deltas.bytecode.types.{TypeSkeleton, UnqualifiedObjectTypeDelta}
import deltas.expression.ExpressionDelta
import deltas.javac.classes.skeleton.HasConstraintsDelta
import deltas.javac.classes.skeleton.JavaClassDelta.ClassImports

object SolidityContractDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object ContractType extends NodeField
  object SuperContracts extends NodeField

  object SuperShape extends NodeShape
  object SuperName extends NodeField
  object SuperArguments extends NodeField

  implicit class ContractLike[T <: NodeLike](val node: T) extends HasName[T] {
    def imports = node(ClassImports).asInstanceOf[Seq[T]]
    def imports_=(value: Seq[T]): Unit = node(ClassImports) = value

    def members = node(JavaClassDelta.Members).asInstanceOf[Seq[T]]
    def members_=(value: Seq[T]): Unit = node(JavaClassDelta.Members) = value

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
    val member = create(JavaClassDelta.Members)
    val members = member.manySeparatedVertical(BlankLine).as(JavaClassDelta.Members)
    val contract = contractType ~~ identifier.as(Name) ~ inheritance ~ "{" % members % "}" asNode JavaClassDelta.Shape
    find(FileWithMembersDelta.Members).addAlternative(contract)
  }

  override def description = "Adds the contract/interface/library"

  override def dependencies = Set(FileWithMembersDelta, UnqualifiedObjectTypeDelta)

  override def shape = JavaClassDelta.Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val contractLike: ContractLike[NodePath] = path
    val contractDeclaration = builder.declare(contractLike.name, parentScope, path.getSourceElement(Name))
    val contractStaticType = TypeFromDeclaration(contractDeclaration)
    val contractInstanceType = TypeFromDeclaration(contractDeclaration) // TODO fix this
    builder.add(DeclarationHasType(contractDeclaration, contractStaticType))
    builder.assignSubType(TypeSkeleton.typeKind, contractStaticType)
    val contractScope = builder.declareScope(contractDeclaration, Some(parentScope), s"contract '${contractLike.name}'")

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



