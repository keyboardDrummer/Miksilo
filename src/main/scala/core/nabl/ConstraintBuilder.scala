package core.nabl

import core.language.SourceElement
import core.nabl.objects.{Declaration, DeclarationVariable, NamedDeclaration, Reference}
import core.nabl.scopes.imports.DeclarationOfScope
import core.nabl.scopes.objects.{ConcreteScope, _}
import core.nabl.scopes.{DeclarationInsideScope, ParentScope, ReferenceInScope}
import core.nabl.types.objects.{Type, TypeVariable}
import core.nabl.types.{CheckSubType, DeclarationOfType, Specialization, TypesAreEqual}

import scala.collection.mutable

case class Copy(key: AnyRef, counter: Int)
class ConstraintBuilder(factory: Factory) {

  val typeVariables: scala.collection.mutable.Map[String, TypeVariable] = mutable.Map.empty   //TODO deze moeten nog resetten

  def scopeVariable(parent: Option[Scope] = None): ScopeVariable = {
    val result = factory.scopeVariable
    parent.foreach(p => constraints ::= ParentScope(result, p))
    result
  }

  def newScope(): ConcreteScope = factory.newScope

  def typeVariable(): TypeVariable = factory.typeVariable

  var constraints: List[Constraint] = List.empty

  def newScope(parent: Option[Scope]) : ConcreteScope = {
    val result = factory.newScope
    parent.foreach(p => constraints ::= ParentScope(result, p))
    result
  }

  def resolve(name: String, origin: SourceElement, scope: Scope, _type: Option[Type] = None) : DeclarationVariable = {
    val result = _type.fold(declarationVariable())(t => declarationVariable(t))
    reference(name, origin, scope, result)
    result
  }

  def reference(name: String, origin: SourceElement, scope: Scope, declaration: Declaration) : Reference = {
    val result = new Reference(name, origin)
    constraints ::= ReferenceInScope(result, scope) //TODO waarom maakt het uit als ik deze twee omdraai?
    constraints ::= ResolvesTo(result, declaration)
    result
  }

  def declarationType(name: String, origin: SourceElement, container: Scope) : Type  = {
    val result = typeVariable()
    declaration(name, origin, container, Some(result))
    result
  }

  def declaration(name: String, origin: SourceElement, container: Scope, _type: Option[Type] = None): NamedDeclaration = {
    val result = new NamedDeclaration(name, origin)
    constraints ::= DeclarationInsideScope(result, container)
    _type.foreach(t => constraints ::= DeclarationOfType(result, t))
    result
  }

  def getCommonSuperType(first: Type, second: Type): Type = {
    val superType = typeVariable()
    checkSubType(superType, first)
    checkSubType(superType, second)
    superType
  }

  def specialization(first: Type, second: Type, debugInfo: Any = null): Unit = add(Specialization(first, second, debugInfo))
  def typesAreEqual(first: Type, second: Type): Unit = add(TypesAreEqual(first, second))

  def add(addition: Constraint): Unit = constraints ::= addition
  def add(addition: List[Constraint]): Unit = constraints = addition ++ constraints

  def declarationVariable(): DeclarationVariable = {
    factory.declarationVariable
  }

  def getType(declaration: Declaration) : Type = {
    val result = typeVariable()
    add(DeclarationOfType(declaration, result))
    result
  }

  def declarationVariable(_type: Type): DeclarationVariable = {
    val result = factory.declarationVariable
    constraints ::= DeclarationOfType(result, _type)
    result
  }

  /*
  Get the scope declared by the given declaration
   */
  def declaredScopeVariable(declaration: Declaration, parent: Option[Scope] = None): ScopeVariable = {
    val result = scopeVariable(parent)
    constraints ::= DeclarationOfScope(declaration, result)
    result
  }

  def declaredNewScope(declaration: Declaration, parent: Option[Scope] = None): ConcreteScope = {
    val result = newScope(parent)
    constraints ::= DeclarationOfScope(declaration, result)
    result
  }

  def getConstraints: Seq[Constraint] = {
    val result = constraints.reverse
    constraints = List.empty
    result
  }

  def checkSubType(superType: Type, subType: Type): Unit = {
    add(CheckSubType(subType, superType))
  }
}
