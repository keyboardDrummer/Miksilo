package deltas.solidity

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.{Keyword, RegexGrammar}
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.node.{Node, NodeLike, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{PrimitiveType, Type, TypeFromDeclaration}
import deltas.HasNameDelta
import deltas.HasNameDelta.HasName
import deltas.bytecode.types.{TypeInstance, TypeSkeleton}
import deltas.javac.types.BooleanTypeDelta

object ElementaryTypeDelta extends DeltaWithGrammar with TypeInstance {
  def neww(value: String): Node = Shape.create(HasNameDelta.Name -> value)

  def getTypeFromString(value: String): Type = ???

  object Shape extends NodeShape

  override def description= "Add elementary types"

  override def dependencies = Set(TypeSkeleton, BooleanTypeDelta)

  override def shape = Shape

  override def getSuperTypes(_type: Node) = ???

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    val javaGrammar: BiGrammar = getJavaGrammar(grammars)
    grammars.create(shape, javaGrammar)
    val parseType = grammars.find(TypeSkeleton.JavaTypeGrammar)
    parseType.addAlternative(javaGrammar, precedence = true)
  }

  val Int = Seq("int", "int8", "int16", "int24", "int32", "int40", "int48", "int56", "int64", "int72", "int80", "int88", "int96", "int104", "int112", "int120", "int128", "int136", "int144", "int152", "int160", "int168", "int176", "int184", "int192", "int200", "int208", "int216", "int224", "int232", "int240", "int248", "int256")

  val unsignedInt = Seq("uint", "uint8", "uint16", "uint24", "uint32", "uint40", "uint48", "uint56", "uint64", "uint72", "uint80", "uint88", "uint96", "uint104", "uint112", "uint120", "uint128", "uint136", "uint144", "uint152", "uint160", "uint168", "uint176", "uint184", "uint192", "uint200", "uint208", "uint216", "uint224", "uint232", "uint240", "uint248", "uint256")

  val Byte = Seq("bytes", "bytes1", "bytes2", "bytes3", "bytes4", "bytes5", "bytes6", "bytes7", "bytes8", "bytes9", "bytes10", "bytes11", "bytes12", "bytes13", "bytes14", "bytes15", "bytes16", "bytes17", "bytes18", "bytes19", "bytes20", "bytes21", "bytes22", "bytes23", "bytes24", "bytes25", "bytes26", "bytes27", "bytes28", "bytes29", "bytes30", "bytes31", "bytes32")

  val fixed = Keyword("fixed", reserved = false) | RegexGrammar("""fixed[0-9]x[0-9]+""".r) ;
  val unsignedFixed = Keyword("ufixed", reserved = false) | RegexGrammar("""ufixed[0-9]x[0-9]+""".r) ;
  val elementaryTypeNames = Seq("address", "string", "var") ++ Int ++ unsignedInt ++ Seq("byte") ++ Byte

  override def getJavaGrammar(grammars: LanguageGrammars) = {
    import grammars._
    find(BooleanTypeDelta.Shape).find(p => p.value.isInstanceOf[Keyword]).get.value.asInstanceOf[Keyword].value = "bool"

    val elementaryTypeName = elementaryTypeNames.map(name => Keyword(name, reserved = false).as(HasNameDelta.Name).asInstanceOf[BiGrammar]).
        reduce((a,b) => a | b) | fixed | unsignedFixed asLabelledNode Shape
    elementaryTypeName
  }

  implicit class ElementType[T <: NodeLike](val node: T) extends HasName[T] {

  }

  val elementaryTypeKind = PrimitiveType("elementaryType")
  override def getType(compilation: Compilation, builder: ConstraintBuilder, path: NodeLike, parentScope: Scope): Type = {
    val elementaryType: ElementType[NodeLike] = path
    if (elementaryType.name == "address") {
      TypeFromDeclaration(builder.resolveToType(elementaryType.name, null, parentScope, TypeSkeleton.typeKind))
    } else {
      PrimitiveType(elementaryType.name)
    }
  }
}
