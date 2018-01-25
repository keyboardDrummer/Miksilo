package core.nabl.constraints

import core.nabl.constraints.objects.{Declaration, DeclarationVariable, NamedDeclaration}
import core.nabl.constraints.scopes._
import core.nabl.constraints.scopes.objects.{ConcreteScope, Scope, ScopeVariable}
import core.nabl.constraints.types.objects._
import core.nabl.constraints.types.{CheckSubType, TypeGraph, TypeNode, TypesAreEqual}

import scala.collection.mutable

/*
Solves an ordered sequence of constraints. Takes a constraint builder because some constraints can create new ones.
The output consists of
- a graph of scopes
- a graph of types
- a mapping of declarations to types.

If constraints generate new ones, how do we guarantee termination?
*/
class ConstraintSolver(val builder: ConstraintBuilder, val startingConstraints: Seq[Constraint],
                       val maxCycles: Int = 100)
{
  val scopeGraph = new ScopeGraph
  val typeGraph = new TypeGraph
  var environment = Map.empty[Declaration, Type]
  var constraints: Seq[Constraint] = startingConstraints
  var mappedTypeVariables: Map[TypeVariable, Type] = Map.empty
  var mappedDeclarationVariables: Map[DeclarationVariable, Declaration] = Map.empty
  var generatedConstraints: Seq[Constraint] = Seq.empty


  def run() : Boolean = {
    var progress = true
    var cycleCount = 0
    while(progress && constraints.nonEmpty && cycleCount < maxCycles)
    {
      progress = cycle()
      cycleCount += 1
    }
    constraints.isEmpty
  }

  /*
  Loop over all the constraints once.
   */
  def cycle() : Boolean = {
    var progress = false
    val remainingConstraints = constraints.filter(constraint => {
      val result = !constraint.apply(this)
      progress |= result
      result
    })
    progress |= generatedConstraints.nonEmpty
    constraints = remainingConstraints ++ generatedConstraints
    generatedConstraints = Seq.empty
    progress
  }

  /*
  Create a declaration, if this declaration already exists, unify the existing and new type.
   */
  def declare(declaration: NamedDeclaration, _type: Type): Boolean = {
    var result = true
    val currentValue: Option[Type] = environment.get(declaration)
    environment = currentValue match {
      case Some(existingType) =>
        if (!unifyTypes(existingType, _type)) {
          result = false
        }
        environment
      case _ => environment + (declaration -> _type)
    }
    result
  }

  def allConstraints: Seq[Constraint] = constraints ++ generatedConstraints

  /*
  Replace a type variable with another type.
   */
  def instantiateType(variable: TypeVariable, _type: Type): Boolean = {
    if (_type.variables.contains(variable))
      return false

    mappedTypeVariables += variable -> _type
    allConstraints.foreach(c => c.instantiateType(variable, _type)) //TODO startingConstraints mag ook gewoon constraints zijn.
    environment = environment.mapValues(existingType => existingType.instantiateType(variable, _type))
    true
  }

  def instantiateScope(variable: ScopeVariable, scope: Scope): Unit = {
    allConstraints.foreach(c => c.instantiateScope(variable, scope))
  }

  def unifyScopes(left: Scope, right: Scope): Boolean = (left, right) match {
    case (variable: ScopeVariable, _) =>
      instantiateScope(variable, right); true
    case (_, variable: ScopeVariable) =>
      instantiateScope(variable, left); true
    case (ConcreteScope(x), ConcreteScope(y)) => if (x == y) true else false
    case _ => false
  }

  /*
  Checks whether the type superType is a super set of the type subType.
   */
  def isSuperType(superType: Type, subType: Type): Boolean = (resolveType(superType), resolveType(subType)) match {
    case (_: TypeVariable,_) => false
    case (_,_: TypeVariable) => false
    case (closure: ConstraintClosureType, FunctionType(input, output, _)) =>
      val closureOutput = closure.instantiate(builder, input)
      builder.add(CheckSubType(output, closureOutput))
      generatedConstraints ++= builder.getConstraints
      true
    case (FunctionType(input, output, _), closure: ConstraintClosureType) =>
      val closureOutput = closure.instantiate(builder, input)
      builder.add(CheckSubType(closureOutput, output))
      generatedConstraints ++= builder.getConstraints
      true
    case (l, r) =>
      typeGraph.isSuperType(TypeNode(l), TypeNode(r))
  }

  def resolveType(_type: Type): Type = _type match {
    case v: TypeVariable => mappedTypeVariables.get(v) match
    {
      case Some(value) => resolveType(value)
      case _ => _type
    }
    case _ => _type
  }

  def unifyTypes(left: Type, right: Type): Boolean = (resolveType(left), resolveType(right)) match {
    case (TypeVariable(nl), TypeVariable(nr)) if nl == nr => true
    case (v: TypeVariable,_) => instantiateType(v,right)
    case (_,v: TypeVariable) => instantiateType(v,left)
    case (closure: ConstraintClosureType, app: TypeApplication) => unifyClosure(closure, app)
    case (app: TypeApplication, closure: ConstraintClosureType) => unifyClosure(closure, app)
    case(StructConstraintType(leftDeclaration), StructConstraintType(rightDeclaration)) =>
      unifyDeclarations(leftDeclaration, rightDeclaration)
    case (PrimitiveType(leftName), PrimitiveType(rightName)) => leftName == rightName
    case (TypeApplication(leftFunction, leftArguments, _), TypeApplication(rightFunction, rightArguments, _)) =>
      if (leftArguments.size == rightArguments.size && unifyTypes(leftFunction, rightFunction))
        leftArguments.indices.forall(index =>
          unifyTypes(left.asInstanceOf[TypeApplication].arguments(index), right.asInstanceOf[TypeApplication].arguments(index)))
      else
        false
    case _ =>
      false
  }

  val unifiedClosures: mutable.Set[(ConstraintClosureType, AnyRef)] = mutable.Set.empty
  def unifyClosure(closure: ConstraintClosureType, typeApplication: TypeApplication): Boolean =
    typeApplication match {
    case FunctionType(input, output, origin) =>
      if (!unifiedClosures.add((closure, origin)))
      {
        return true
      }
      val closureOutput = closure.instantiate(builder, input)
      builder.add(TypesAreEqual(closureOutput, output))
      generatedConstraints ++= builder.getConstraints
      true
    case _ => false
  }

  def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
    allConstraints.foreach(x => x.instantiateDeclaration(variable, instance))
    environment = environment.map(kv => if (kv._1 == variable) (instance, kv._2) else kv)
    environment.values.foreach(t => t.instantiateDeclaration(variable, instance))
    mappedDeclarationVariables += variable -> instance
  }

  def unifyDeclarations(left: Declaration, right: Declaration): Boolean = (left, right) match {
    case (v:DeclarationVariable,_) =>
      instantiateDeclaration(v, right); true
    case (_, v:DeclarationVariable) =>
      instantiateDeclaration(v, left); true
    case _ => left == right
  }

  def boundVariables : Set[TypeVariable] = {
    val constraintTypes = allConstraints.flatMap(c => c.boundTypes)
    constraintTypes.flatMap(t => t.variables).toSet
  }
}
