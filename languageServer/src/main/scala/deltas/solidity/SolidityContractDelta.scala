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
import deltas.ConstraintSkeleton
import deltas.bytecode.types.UnqualifiedObjectTypeDelta
import deltas.expression.ExpressionDelta
import deltas.javac.classes.skeleton.{HasConstraintsDelta, JavaClassSkeleton}
import deltas.javac.classes.skeleton.JavaClassSkeleton.ClassImports

object SolidityContractDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object Shape extends NodeShape
  object ContractType extends NodeField
  object SuperContracts extends NodeField
  object Members extends NodeField

  object SuperShape extends NodeShape
  object SuperName extends NodeField
  object SuperArguments extends NodeField

  implicit class ContractLike[T <: NodeLike](val node: T) extends AnyVal {
    def imports = node(ClassImports).asInstanceOf[Seq[T]]
    def imports_=(value: Seq[T]): Unit = node(ClassImports) = value

    def name: String = node.getValue(JavaClassSkeleton.Name).asInstanceOf[String]
    def name_=(value: String): Unit = node(JavaClassSkeleton.Name) = value

    def members = node(Members).asInstanceOf[Seq[T]]
    def members_=(value: Seq[T]): Unit = node(Members) = value

    def parents: Seq[T] = node(SuperContracts).asInstanceOf[Seq[T]]
    def parents_=(value: Seq[T]): Unit = node(SuperContracts) = value
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val contractType = ("contract" | "interface" | "library").as(ContractType)
    val objectType = find(UnqualifiedObjectTypeDelta.AnyObjectTypeGrammar)
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val inheritanceSpecifier: BiGrammar = objectType.as(SuperName) ~
      (expression.someSeparated("," ~ printSpace).inParenthesis | value(Seq.empty)).as(SuperArguments) asNode SuperShape
    val inheritance = (printSpace ~ "is" ~~ inheritanceSpecifier.someSeparated("," ~ printSpace) | value(Seq.empty)).as(SuperContracts)
    val member = create(Members)
    val members = member.manySeparatedVertical(BlankLine).as(Members)
    val contract = contractType ~~ identifier.as(JavaClassSkeleton.Name) ~ inheritance ~ "{" % members % "}" asNode Shape
    find(FileWithMembersDelta.Members).addAlternative(contract)
  }

  override def description = "Adds the contract/interface/library"

  override def dependencies = Set(FileWithMembersDelta, UnqualifiedObjectTypeDelta)

  override def shape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val contractLike: ContractLike[NodePath] = path
    val contractDeclaration = builder.declare(contractLike.name, parentScope, path.getSourceElement(JavaClassSkeleton.Name))
    val contractScope = builder.declareScope(contractDeclaration, Some(parentScope), s"contract '${contractLike.name}'")

    for(member <- contractLike.members) {
      ConstraintSkeleton.constraints(compilation, builder, member, contractScope)
    }
  }
}



