package miksilo.modularLanguages.deltas.solidity

import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.{FieldPath, NodePath}
import miksilo.modularLanguages.core.node._
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.objects.{Declaration, DeclarationVariable, NamedDeclaration}
import miksilo.languageServer.core.smarts.scopes.objects.{Scope, ScopeVariable}
import miksilo.languageServer.core.smarts.{Constraint, ConstraintBuilder, ConstraintSolver}
import miksilo.modularLanguages.deltas.HasNameDelta.Name
import miksilo.modularLanguages.deltas.bytecode.types.TypeSkeleton
import miksilo.modularLanguages.deltas.solidity.SolidityContractDelta.ContractLike
import miksilo.modularLanguages.deltas.solidity.SolidityFunctionDelta.ReturnValues
import miksilo.modularLanguages.core.deltas.path.ConstraintBuilderExtension._
import miksilo.modularLanguages.deltas.classes.ClassDelta
import miksilo.modularLanguages.deltas.classes.HasConstraintsDelta
import miksilo.modularLanguages.deltas.javac.methods.MethodParameters
import miksilo.modularLanguages.deltas.javac.methods.MethodParameters.MethodParameter
import miksilo.modularLanguages.deltas.method.MethodDelta.Method

object UsingForDeclarationDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object Shape extends NodeShape
  object LibraryName extends NodeField
  object Type extends NodeField
  object Wildcard

  implicit class UsingFor[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def libraryName: String = node.getValue(LibraryName).asInstanceOf[String]
    def libraryName_=(value: String): Unit = node(LibraryName) = value

    def _type: T = node(Type).asInstanceOf[T]
    def _type_=(value: T): Unit = node(Type) = value
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    val grammar = "using" ~~ identifier.as(LibraryName) ~~ "for" ~~
      ("*" ~> value(Wildcard) | typeGrammar).as(Type) ~ ";" asNode Shape
    find(ClassDelta.Members).addAlternative(grammar)
  }

  override def description = "Add a using-for namespace member"

  override def dependencies = Set(SolidityContractDelta, TypeSkeleton)

  override def shape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val usingFor: UsingFor[NodePath] = path
    val _type = TypeSkeleton.getType(compilation, builder, usingFor._type, parentScope)
    val libraryDeclaration: DeclarationVariable = builder.resolve(usingFor.libraryName, parentScope, usingFor.getField(LibraryName))
    val libraryScope = builder.getDeclaredScope(libraryDeclaration)

    val typeDeclaration = builder.getDeclarationOfType(_type)
    val typeScope = builder.getDeclaredScope(typeDeclaration)
    builder.add(new UsingForConstraint(compilation, typeScope, libraryScope, libraryDeclaration))
  }

  class UsingForConstraint(compilation: Compilation,
                           var typeScope: Scope,
                           var libraryScope: Scope,
                           var libraryDeclaration: Declaration) extends Constraint {

    override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
      if (libraryDeclaration == variable)
        libraryDeclaration = instance
      super.instantiateDeclaration(variable, instance)
    }

    override def instantiateScope(variable: ScopeVariable, instance: Scope): Unit = {
      if (typeScope == variable)
        typeScope = instance
      if (libraryScope == variable)
        libraryScope = instance
      super.instantiateScope(variable, instance)
    }

    override def apply(solver: ConstraintSolver): Boolean = {
      libraryDeclaration match {
        case d: NamedDeclaration =>
          val library: ContractLike[NodePath] = d.origin.get.asInstanceOf[FieldPath].parent
          for(member <- library.members) {
            if (member.shape == SolidityFunctionDelta.shape) {

              val method: Method[NodePath] = member

              val parameterTypes = method.parameters.map(p => p(MethodParameters.Type).asInstanceOf[NodePath]).drop(1)
              val returnParameters: Seq[MethodParameter[NodePath]] = NodeWrapper.wrapList(method(ReturnValues).asInstanceOf[Seq[NodePath]])
              val returnTypes: Seq[Node] = returnParameters.map(returnParameter => returnParameter._type)
              val methodType = SolidityFunctionTypeDelta.createType(compilation, solver.builder, libraryScope, parameterTypes, returnTypes)

              solver.builder.declare(member.getField(Name), typeScope, methodType)
            }
          }
          true
        case _ => false
      }
    }
  }
}


