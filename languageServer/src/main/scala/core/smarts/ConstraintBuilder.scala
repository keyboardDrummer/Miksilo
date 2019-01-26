package core.smarts

import core.language.SourceElement
import core.smarts.objects.{Declaration, DeclarationVariable, NamedDeclaration, Reference}
import core.smarts.scopes.imports.DeclarationOfScope
import core.smarts.scopes.objects.{ConcreteScope, _}
import core.smarts.scopes.{DeclarationInsideScope, ParentScope, ReferenceInScope}
import core.smarts.types.objects.{Type, TypeFromDeclaration, TypeVariable}
import core.smarts.types._

import scala.collection.mutable

case class Copy(key: AnyRef, counter: Int)
class ConstraintBuilder(val factory: Factory) {

  var proofs: Proofs = null

  val typeVariables: scala.collection.mutable.Map[String, TypeVariable] = mutable.Map.empty   //TODO deze moeten nog resetten

  def scopeVariable(parent: Option[Scope] = None, name: Any = null): ScopeVariable = {
    val result = factory.scopeVariable(name)
    parent.foreach(p => constraints ::= ParentScope(result, p))
    result
  }

  def typeVariable(origin: Option[SourceElement] = None): TypeVariable = factory.typeVariable(origin)

  var constraints: List[Constraint] = List.empty

  def newScope(parent: Option[Scope] = None, debugName: String = "") : ConcreteScope = {
    val result = factory.newScope(debugName)
    parent.foreach(p => add(ParentScope(result, p)))
    result
  }

  def importScope(into: Scope, source: Scope): Unit = add(ParentScope(into, source))

  def resolveToType(name: String, origin: SourceElement, scope: Scope, _type: Type) : DeclarationVariable = {
    val declaration = declarationVariable()
    val reference = new Reference(name, Some(origin))
    add(ReferenceInScope(reference, scope))
    add(new ResolvesToType(reference, declaration, _type))
    declaration
  }

  def resolve(name: String, origin: SourceElement, scope: Scope, _type: Option[Type] = None) : DeclarationVariable = {
    resolveOption(name, Some(origin), scope, _type)
  }

  def resolveOption(name: String, origin: Option[SourceElement], scope: Scope, _type: Option[Type] = None): DeclarationVariable = {
    val declaration = _type.fold(declarationVariable())(t => declarationVariable(t))
    val reference = refer(name, scope, origin)
    constraints ::= ResolvesTo(reference, declaration)
    declaration
  }

  def refer(name: String, scope: Scope, origin: Option[SourceElement]): Reference = {
    val result = new Reference(name, origin)
    constraints ::= ReferenceInScope(result, scope)
    result
  }

  def declareSourceElement(name: SourceElement, container: Scope, _type: Option[Type] = None): NamedDeclaration = {
    declare(name.current.asInstanceOf[String], container, name, _type)
  }

  def declare(name: String, container: Scope, origin: SourceElement = null, _type: Option[Type] = None): NamedDeclaration = { //TODO the order here is inconsistent with resolve.
    val result = new NamedDeclaration(name, Option(origin))
    constraints ::= DeclarationInsideScope(result, container)
    _type.foreach(t => constraints ::= DeclarationHasType(result, t))
    result
  }

  def getCommonSuperType(first: Type, second: Type): Type = { //TODO this doesn't actually work, because it won't find the superType. We need a separate constraint.
    val superType = typeVariable()
    typesAreEqual(first, superType)
    isFirstSubsetOfSecond(second, superType)
    superType
  }

  def specialization(first: Type, second: Type, debugInfo: Any = null): Unit = add(Specialization(first, second, debugInfo))
  def typesAreEqual(first: Type, second: Type): Unit = add(TypesAreEqual(first, second))

  def add(addition: Constraint): Unit = constraints ::= addition
  def add(addition: List[Constraint]): Unit = constraints = addition ++ constraints

  def assignSubType(superType: Type, subType: Type) = add(AssignSubType(subType, superType))

  def declarationVariable(name: Option[Any] = None): DeclarationVariable = {
    factory.declarationVariable(name)
  }

  def getDeclarationOfType(_type: Type, declarationName: Option[Any] = None): Declaration = {
    val result = declarationVariable(declarationName)
    add(TypesAreEqual(TypeFromDeclaration(result), _type))
    result
  }

  def getType(declaration: Declaration) : Type = {
    val result = typeVariable()
    add(DeclarationHasType(declaration, result))
    result
  }

  def declarationVariable(_type: Type): DeclarationVariable = {
    val result = factory.declarationVariable()
    constraints ::= DeclarationHasType(result, _type)
    result
  }

  /*
  Get the scope declared by the given declaration
   */
  def getDeclaredScope(declaration: Declaration, scopeName: Any = null): ScopeVariable = {
    val result = scopeVariable(None, scopeName)
    constraints ::= DeclarationOfScope(declaration, result)
    result
  }

  def declareScope(declaration: Declaration, parent: Option[Scope] = None, debugName: String = ""): ConcreteScope = {
    val result = newScope(parent, debugName)
    constraints ::= DeclarationOfScope(declaration, result)
    result
  }

  def getConstraints: Seq[Constraint] = {
    val result = constraints.reverse
    constraints = List.empty
    result
  }

  def isFirstSubsetOfSecond(subType: Type, superType: Type): Unit = {
    add(CheckSubType(subType, superType))
  }

  def toSolver: ConstraintSolver = {
    new ConstraintSolver(this, getConstraints, proofs = if (proofs != null) proofs else new Proofs())
  }
}
