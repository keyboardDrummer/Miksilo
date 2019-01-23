package deltas.javac.classes

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.{Compilation, Language}
import core.smarts.objects.Reference
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import core.smarts.{ConstraintBuilder, ResolvesTo}
import deltas.expression.{ExpressionDelta, ExpressionInstance}
import deltas.javac.methods.MemberSelectorDelta._
import deltas.javac.methods.call.ReferenceExpressionDelta
import deltas.javac.methods.{HasScopeSkeleton, MemberSelectorDelta}

object SelectFieldDelta extends DeltaWithGrammar with ExpressionInstance with ReferenceExpressionDelta {

  override def description: String = "Enables using the . operator to select a field from a class."

  override val shape = Shape

  override def dependencies: Set[Contract] = Set(MemberSelectorDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val core = grammars.find(ExpressionDelta.LastPrecedenceGrammar)
    core.addAlternative(grammars.find(MemberSelectorDelta.Shape))
  }

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, selector: NodePath, _type: Type, parentScope: Scope): Unit = {
    val declaration = builder.declarationVariable(_type)
    val reference = getReference(compilation, builder, selector, parentScope)
    builder.add(ResolvesTo(reference, declaration))
  }

  override def getReference(compilation: Compilation, builder: ConstraintBuilder, selector: NodePath, parentScope: Scope): Reference = {
    val target = selector.target
    val scope = HasScopeSkeleton.getScope(compilation, builder, target, parentScope)
    val member = selector.member
    builder.refer(member, scope, Some(selector.getSourceElement(Member)))
  }
}
