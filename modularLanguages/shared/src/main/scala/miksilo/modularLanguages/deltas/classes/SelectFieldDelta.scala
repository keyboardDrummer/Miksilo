package miksilo.modularLanguages.deltas.javac.classes

import miksilo.modularLanguages.core.deltas._
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.objects.Reference
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.Type
import miksilo.languageServer.core.smarts.{ConstraintBuilder, ResolvesTo}
import miksilo.modularLanguages.deltas.expression.{ExpressionDelta, ExpressionInstance}
import miksilo.modularLanguages.deltas.javac.methods.MemberSelectorDelta
import miksilo.modularLanguages.deltas.javac.methods.MemberSelectorDelta.{Member, MemberSelector, Shape}
import miksilo.modularLanguages.deltas.javac.methods.call.ReferenceExpressionDelta

object SelectFieldDelta extends DeltaWithGrammar with ExpressionInstance with ReferenceExpressionDelta {

  override def description: String = "Enables using the . operator to select a field from a class."

  override val shape = MemberSelectorDelta.Shape

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

  override def getReference(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Reference = {
    val selector: MemberSelector[NodePath] = path
    val _type = ExpressionDelta.getType(compilation, builder, selector.target, parentScope)
    val declaration = builder.getDeclarationOfType(_type)
    val scope = builder.getDeclaredScope(declaration)
    val member = selector.member
    builder.refer(member, scope, Some(selector.getField(Member)))
  }
}
