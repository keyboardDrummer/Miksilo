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
class ConstraintBuilder(factory: Factory) {

  var proofs: Proofs = null

  val typeVariables: scala.collection.mutable.Map[String, TypeVariable] = mutable.Map.empty   //TODO deze moeten nog resetten

  def scopeVariable(parent: Option[Scope] = None): ScopeVariable = {
    val result = factory.scopeVariable
    parent.foreach(p => constraints ::= ParentScope(result, p))
    result
  }

  def typeVariable(): TypeVariable = factory.typeVariable

  var constraints: List[Constraint] = List.empty

  def newScope(parent: Option[Scope] = None, debugName: String = "") : ConcreteScope = {
    val result = factory.newScope(debugName)
    parent.foreach(p => add(ParentScope(result, p)))
    result
  }

  def importScope(into: Scope, source: Scope): Unit = add(ParentScope(into, source))

  def resolve(name: String, origin: SourceElement, scope: Scope, _type: Option[Type] = None) : DeclarationVariable = {
    resolveOption(name, Some(origin), scope, _type)
  }

  def resolveOption(name: String, origin: Option[SourceElement], scope: Scope, _type: Option[Type] = None): DeclarationVariable = {
    val result = _type.fold(declarationVariable())(t => declarationVariable(t))
    reference(name, origin, scope, result)
    result
  }

  def reference(name: String, scope: Scope, declaration: Declaration) : Reference = {
    reference(name, None, scope, declaration)
  }

  def reference(name: String, origin: SourceElement, scope: Scope, declaration: Declaration) : Reference = {
    reference(name, Some(origin), scope, declaration)
  }

  private def reference(name: String, origin: Option[SourceElement], scope: Scope, declaration: Declaration) : Reference = {
    val result = new Reference(name, origin)
    constraints ::= ReferenceInScope(result, scope) //TODO waarom maakt het uit als ik deze twee omdraai?
    constraints ::= ResolvesTo(result, declaration)
    result
  }

  def declare(name: String, origin: SourceElement, container: Scope, _type: Option[Type] = None): NamedDeclaration = {
    val result = new NamedDeclaration(name, origin)
    constraints ::= DeclarationInsideScope(result, container)
    _type.foreach(t => constraints ::= DeclarationHasType(result, t))
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

  def assignSubType(superType: Type, subType: Type) = add(AssignSubType(subType, superType))

  def declarationVariable(): DeclarationVariable = {
    factory.declarationVariable
  }

  def getTypeDeclaration(_type: Type): Declaration = {
    val result = declarationVariable()
    add(TypesAreEqual(TypeFromDeclaration(result), _type))
    result
  }

  def getType(declaration: Declaration) : Type = {
    val result = typeVariable()
    add(DeclarationHasType(declaration, result))
    result
  }

  def declarationVariable(_type: Type): DeclarationVariable = {
    val result = factory.declarationVariable
    constraints ::= DeclarationHasType(result, _type)
    result
  }

  /*
  Get the scope declared by the given declaration
   */
  def getDeclaredScope(declaration: Declaration): ScopeVariable = {
    val result = scopeVariable()
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

  def checkSubType(superType: Type, subType: Type): Unit = {
    add(CheckSubType(subType, superType))
  }

  def toSolver: ConstraintSolver = {
    new ConstraintSolver(this, getConstraints, proofs = if (proofs != null) proofs else new Proofs())
  }
}
