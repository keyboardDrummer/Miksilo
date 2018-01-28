package deltas.javac.expressions.postfix

import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeField, NodeShape}
import core.deltas.path.{NodePath, Path}
import core.deltas.{Compilation, Contract}
import core.language.Language
import core.nabl.ConstraintBuilder
import core.nabl.scopes.objects.Scope
import core.nabl.types.objects.Type
import deltas.bytecode.coreInstructions.integers.IncrementIntegerDelta
import deltas.bytecode.types.IntTypeDelta
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import deltas.javac.methods.MethodDelta

object PostFixIncrementDelta extends ExpressionInstance {

  override val key = PostfixIncrementKey

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton, MethodDelta, IncrementIntegerDelta)

  override def getType(expression: NodePath, compilation: Compilation): Node = IntTypeDelta.intType

  override def toByteCode(plusPlus: NodePath, compilation: Compilation): Seq[Node] = {
    val methodCompiler = MethodDelta.getMethodCompiler(compilation)
    val name: Path = plusPlus(VariableKey).asInstanceOf[Path]
    ???
//    val variableAddress = methodCompiler.bindingsAndTypes.scopes.resolveLocation(name).asInstanceOf[Path]
//    Seq(LoadIntegerDelta.load(variableAddress), IncrementIntegerDelta.integerIncrement(variableAddress, 1))
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val coreGrammar = find(ExpressionSkeleton.CoreGrammar)
    val postFixIncrement = identifier.as(VariableKey) ~< "++" asNode PostfixIncrementKey
    coreGrammar.addOption(postFixIncrement)
  }

  object PostfixIncrementKey extends NodeShape

  object VariableKey extends NodeField //TODO this can also be a member instead of just an identifier.

  override def description: String = "Adds the postfix ++ operator."

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, plusPlus: NodePath, _type: Type, parentScope: Scope): Unit = {
    builder.typesAreEqual(IntTypeDelta.constraintType, _type)
    val namePath: Path = plusPlus(VariableKey).asInstanceOf[Path]
    builder.resolve(namePath.current.asInstanceOf[String], namePath, parentScope, Some(_type))
  }
}
