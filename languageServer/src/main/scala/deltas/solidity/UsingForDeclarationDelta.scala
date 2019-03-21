package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.{FieldPath, NodePath}
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.objects.{Declaration, DeclarationVariable, NamedDeclaration}
import core.smarts.scopes.objects.{Scope, ScopeVariable}
import core.smarts.{Constraint, ConstraintBuilder, ConstraintSolver}
import deltas.HasNameDelta.Name
import deltas.bytecode.types.TypeSkeleton
import deltas.javac.classes.skeleton.{HasConstraintsDelta, JavaClassDelta}
import deltas.javac.methods.MethodDelta.Method
import deltas.javac.methods.MethodParameters
import deltas.javac.methods.MethodParameters.MethodParameter
import deltas.solidity.SolidityContractDelta.ContractLike
import deltas.solidity.SolidityFunctionDelta.ReturnValues

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
    find(JavaClassDelta.Members).addAlternative(grammar)
  }

  override def description = "Add a using-for namespace member"

  override def dependencies = Set(SolidityContractDelta, TypeSkeleton)

  override def shape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val usingFor: UsingFor[NodePath] = path
    val _type = TypeSkeleton.getType(compilation, builder, usingFor._type, parentScope)
    val libraryDeclaration: DeclarationVariable = builder.resolve(usingFor.libraryName, usingFor.getSourceElement(LibraryName), parentScope)
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

              solver.builder.declareSourceElement(member.getSourceElement(Name), typeScope, Some(methodType))
            }
          }
          true
        case _ => false
      }
    }
  }
}


