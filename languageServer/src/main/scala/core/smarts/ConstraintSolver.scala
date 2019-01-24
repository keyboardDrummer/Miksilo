package core.smarts

import core.smarts.objects.{Declaration, DeclarationVariable, NamedDeclaration}
import core.smarts.scopes._
import core.smarts.scopes.objects.{ConcreteScope, Scope, ScopeVariable}
import core.smarts.types.objects._
import core.smarts.types.{CheckSubType, TypeGraph, TypeNode, TypesAreEqual}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/*
Solves an ordered sequence of constraints. Takes a constraint builder because some constraints can create new ones.
The output consists of
- a graph of scopes
- a graph of types
- a mapping of declarations to types.

If constraints generate new ones, how do we guarantee termination?
*/
class ConstraintSolver(val builder: ConstraintBuilder, val startingConstraints: Seq[Constraint],
                       val maxCycles: Int = 100,
                       val allowDuplicateDeclaration: Boolean = true, //TODO find out why false causes many tests to fail.
                       val proofs: Proofs = new Proofs()
                      )
{
  def scopeGraph: ScopeGraph = proofs.scopeGraph
  def typeGraph: TypeGraph = proofs.typeGraph
  def environment: Map[Declaration, Type] = proofs.environment
  def environment_=(value: Map[Declaration, Type]): Unit = proofs.environment = value

  var constraints: Seq[Constraint] = startingConstraints
  var generatedConstraints: Seq[Constraint] = Seq.empty

  def run() : Try[Unit] = {
    try
    {
      var progress = true
      var cycleCount = 0
      while(progress && constraints.nonEmpty && cycleCount < maxCycles)
      {
        progress = cycle()
        cycleCount += 1
      }
      if (constraints.isEmpty)
        Success(())
      else if (cycleCount == maxCycles)
        Failure(MaxCycleCountReached(maxCycles))
      else
        Failure(CouldNotApplyConstraints(constraints))
    } catch {
      case e:SolveException => Failure(e)
    }
  }

  /*
  Loop over all the constraints once.
   */
  def cycle() : Boolean = {
    var progress = false
    val remainingConstraints = constraints.filter(constraint => {
      val applied = constraint.apply(this)
      progress |= applied
      !applied
    })
    generatedConstraints ++= builder.getConstraints //TODO add a test for this line.
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
          result = false //TODO Maybe throw an error?
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

    proofs.mappedTypeVariables += variable -> _type
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
    case (ConcreteScope(x, _), ConcreteScope(y, _)) => if (x == y) true else false
    case _ => false
  }

  /*
  Checks whether the type superType is a super set of the type subType.
   */
  def isSuperType(superType: Type, subType: Type): Boolean = (proofs.resolveType(superType), proofs.resolveType(subType)) match {
    case (_: TypeVariable,_) => false
    case (_,_: TypeVariable) => false
    case (closure: ConstraintClosureType, FunctionType(input, output, _)) =>
      val closureOutput = closure.instantiate(builder, input)
      builder.add(CheckSubType(output, closureOutput))
      generatedConstraints ++= builder.getConstraints //TODO shouldn't this be running in a subSolver?
      true
    case (FunctionType(input, output, _), closure: ConstraintClosureType) =>
      val closureOutput = closure.instantiate(builder, input)
      builder.add(CheckSubType(closureOutput, output))
      generatedConstraints ++= builder.getConstraints
      true
    case (l, r) =>
      typeGraph.isSuperType(TypeNode(l), TypeNode(r))
  }

  def canDeclarationsMatch(left: Declaration, right: Declaration) = (left, right) match {
    case (_: DeclarationVariable, _) => true
    case (_, _: DeclarationVariable) => true
    case _ => left == right
  }

  def couldBeSuperType(superType: Type, subType: Type): Boolean = (proofs.resolveType(superType), proofs.resolveType(subType)) match {
    case (_: TypeVariable,_) => true
    case (_,_: TypeVariable) => true
    case (TypeFromDeclaration(superDeclaration), TypeFromDeclaration(subDeclaration)) =>
      canDeclarationsMatch(superDeclaration, subDeclaration) // TODO add test case.
    case (FunctionType(input1, output1, _), FunctionType(input2, output2, _)) =>
      couldBeSuperType(output1, output2) && couldBeSuperType(input2, input1)
    case (closure: ConstraintClosureType, FunctionType(input, output, _)) =>
      val closureOutput = closure.instantiate(builder, input)
      builder.add(CheckSubType(output, closureOutput))
      generatedConstraints ++= builder.getConstraints //TODO shouldn't this be running in a subSolver?
      true
    case (FunctionType(input, output, _), closure: ConstraintClosureType) =>
      val closureOutput = closure.instantiate(builder, input)
      builder.add(CheckSubType(closureOutput, output))
      generatedConstraints ++= builder.getConstraints
      true
    case (l, r) =>
      typeGraph.isSuperType(TypeNode(l), TypeNode(r))
  }

  def unifyTypes(left: Type, right: Type): Boolean = (proofs.resolveType(left), proofs.resolveType(right)) match {
    case (TypeVariable(nl, _), TypeVariable(nr, _)) if nl == nr => true
    case (v: TypeVariable,_) => instantiateType(v,right)
    case (_,v: TypeVariable) => instantiateType(v,left)
    case (closure: ConstraintClosureType, app: TypeApplication) => unifyClosure(closure, app)
    case (app: TypeApplication, closure: ConstraintClosureType) => unifyClosure(closure, app)
    case(TypeFromDeclaration(leftDeclaration), TypeFromDeclaration(rightDeclaration)) =>
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
    proofs.mappedDeclarationVariables += variable -> instance
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
