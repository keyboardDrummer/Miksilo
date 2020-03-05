package deltas.javac.classes

import core.bigrammar.BiGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.node.{GrammarKey, TypedNodeField}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.objects.Declaration
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.TypeFromDeclaration
import deltas.ConstraintSkeleton
import deltas.expression.VariableDelta
import deltas.expression.VariableDelta._
import deltas.HasNameDelta.Name
import deltas.classes.ClassDelta
import deltas.classes.ClassDelta.JavaClass
import deltas.javac.classes.skeleton.{HasConstraints, JavaClassDelta}

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
