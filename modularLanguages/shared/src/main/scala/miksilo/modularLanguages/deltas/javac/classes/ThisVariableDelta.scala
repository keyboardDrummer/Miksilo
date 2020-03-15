package miksilo.modularLanguages.deltas.javac.classes

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.modularLanguages.core.node.{GrammarKey, TypedNodeField}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.objects.Declaration
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.TypeFromDeclaration
import miksilo.modularLanguages.deltas.ConstraintSkeleton
import miksilo.modularLanguages.deltas.expression.VariableDelta
import miksilo.modularLanguages.deltas.expression.VariableDelta._
import miksilo.modularLanguages.deltas.HasNameDelta.Name
import miksilo.modularLanguages.deltas.classes.ClassDelta
import miksilo.modularLanguages.deltas.classes.ClassDelta.JavaClass
import miksilo.modularLanguages.deltas.javac.classes.skeleton.{HasConstraints, JavaClassDelta}

object ThisVariableDelta extends DeltaWithGrammar
{
  object Grammar extends GrammarKey

  val thisName = "this"

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val variable = find(VariableDelta.Shape)
    val thisGrammar = create(Grammar, (thisName: BiGrammar).as(Name)).asNode(Shape)
    variable.addAlternative(thisGrammar)
  }

  val thisDeclarationField = new TypedNodeField[Declaration]("thisDeclaration")
  override def inject(language: Language): Unit = {
    ConstraintSkeleton.hasConstraints.add(language, ClassDelta.Shape, new HasConstraints {
      override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
        val classScope = JavaClassDelta.getClassScope(compilation, builder, path, parentScope)
        val clazz: JavaClass[NodePath] = path
        val clazzName = clazz.name
        val classDeclaration = builder.resolveOption(clazzName, None, classScope)
        val thisDeclaration = builder.declare(thisName, classScope, _type = Some(TypeFromDeclaration(classDeclaration)))
        thisDeclarationField(path) = thisDeclaration
      }
    })
    super.inject(language)
  }

  override def dependencies: Set[Contract] = Set(VariableDelta)

  override def description: String = "Enables using the '" + thisName + "' qualifier to refer to the current instance."
}
