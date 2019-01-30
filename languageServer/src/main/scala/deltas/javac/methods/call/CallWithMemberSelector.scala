package deltas.javac.methods.call

import core.deltas.path.NodePath
import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.objects.Reference
import core.smarts.scopes.objects.Scope
import deltas.javac.expressions.ToByteCodeSkeleton
import deltas.javac.methods.MemberSelectorDelta
import deltas.javac.methods.call.CallDelta.Call

object ReferenceExpressionSkeleton {
  val instances = new ShapeProperty[ReferenceExpression]
  val references = new TypedNodeField[Reference]("definedReference")

  def getReference(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, parentScope: Scope): Reference = {
    val result = instances(compilation, expression.shape).getReference(compilation, builder, expression, parentScope)
    references(expression) = result
    result
  }
}

trait ReferenceExpression {
  def getReference(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, parentScope: Scope): Reference
}

trait ReferenceExpressionDelta extends Delta with HasShape with ReferenceExpression {
  override def inject(language: Language): Unit = {
    super.inject(language)
    ReferenceExpressionSkeleton.instances.add(language, shape, this)
  }
}

trait CallWithMemberSelector extends DeltaWithGrammar {

  override def dependencies: Set[Contract] = Set(CallDelta, MemberSelectorDelta)

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    find(CallDelta.Callee).addAlternative(find(MemberSelectorDelta.Shape))
  }

  def getGenericCallInstructions(call: Call[NodePath], compilation: Compilation, calleeInstructions: Seq[Node], invokeInstructions: Seq[Node]): Seq[Node] = {
    val expressionToInstruction = ToByteCodeSkeleton.getToInstructions(compilation)
    val callArguments = call.arguments
    val argumentInstructions = callArguments.flatMap(argument => expressionToInstruction(argument))
    calleeInstructions ++ argumentInstructions ++ invokeInstructions
  }
}
