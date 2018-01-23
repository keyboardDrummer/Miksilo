package core.nabl.constraints

import core.nabl.constraints.objects.{Declaration, DeclarationVariable, NamedDeclaration}
import core.nabl.constraints.scopes._
import core.nabl.constraints.scopes.objects.{ConcreteScope, Scope, ScopeVariable}
import core.nabl.constraints.types.objects._
import core.nabl.constraints.types.{CheckSubType, TypeGraph, TypeNode}

import scala.collection.mutable

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
    var cycles = 9
    while(progress && constraints.nonEmpty && cycles < maxCycles)
    {
      progress = cycle()
      cycles += 1
    }
    constraints.isEmpty
  }

  def cycle() : Boolean = {
    val remainingConstraints = constraints.filter(c =>
      !c.apply(this)
    )
    val result = constraints.size > remainingConstraints.size || generatedConstraints.nonEmpty
    constraints = remainingConstraints ++ generatedConstraints
    generatedConstraints = Seq.empty
    result
  }

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

  def instantiateType(v: TypeVariable, t: Type): Boolean = {
    if (t.variables.contains(v))
      return false

    mappedTypeVariables += v -> t
    allConstraints.foreach(c => c.instantiateType(v, t)) //TODO startingConstraints mag ook gewoon constraints zijn.
    environment = environment.mapValues(existingType => existingType.instantiateType(v, t))
    true
  }

  def instantiateScope(v: ScopeVariable, s: Scope): Unit = {
    allConstraints.foreach(c => c.instantiateScope(v, s))
  }

  def unifyScopes(left: Scope, right: Scope): Boolean = (left, right) match {
    case (v: ScopeVariable, _) =>
      instantiateScope(v, right); true
    case (_, v: ScopeVariable) =>
      instantiateScope(v, left); true
    case (ConcreteScope(x), ConcreteScope(y)) => if (x == y) true else false
    case _ => false
  }

  def canAssignTo(target: Type, value: Type): Boolean = (resolveType(target), resolveType(value)) match {
    case (v: TypeVariable,_) => false
    case (_,v: TypeVariable) => false
    case (closure: ConstraintClosureType, app: TypeApplication) => canAssignClosure(closure, app)
    case (app: TypeApplication, closure: ConstraintClosureType) => canAssignClosure(closure, app)
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

  def canAssignClosure(closure: ConstraintClosureType, typeApplication: TypeApplication): Boolean = typeApplication match {
    case TypeApplication(PrimitiveType("Func"), Seq(input, output), _) =>
      val bodyScope = builder.newScope(Some(closure.parentScope))
      builder.declaration(closure.name, closure.id, bodyScope, Some(input))
      val actualOutput = closure.body.getType(builder, bodyScope)
      builder.add(CheckSubType(actualOutput, output))
      generatedConstraints ++= builder.getConstraints
      true
    case _ => false
  }

  val unifiedClosures: mutable.Set[(ConstraintClosureType, AnyRef)] = mutable.Set.empty
  def unifyClosure(closure: ConstraintClosureType, typeApplication: TypeApplication): Boolean = typeApplication match {
    case TypeApplication(PrimitiveType("Func"), Seq(input, output), origin) =>
      if (!unifiedClosures.add((closure, origin)))
      {
        return true
      }
      val bodyScope = builder.newScope(Some(closure.parentScope))
      builder.declaration(closure.name, closure.id, bodyScope, Some(input))
      closure.body.constraints(builder, output, bodyScope)
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
