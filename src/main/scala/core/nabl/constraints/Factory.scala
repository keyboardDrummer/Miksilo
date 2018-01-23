package core.nabl.constraints

import core.nabl.constraints.objects.DeclarationVariable
import core.nabl.constraints.scopes.objects.{ConcreteScope, ScopeVariable}
import core.nabl.constraints.types.objects.TypeVariable

/**
  */
class Factory
{
  private var scopeVariableCounter: Int = 0
  def scopeVariable: ScopeVariable = {
    scopeVariableCounter += 1
    ScopeVariable(scopeVariableCounter.toString)
  }

  private var scopeCounter: Int = 0
  def newScope: ConcreteScope = {
    scopeCounter += 1
    ConcreteScope(scopeCounter)
  }

  private var typeCounter = 0
  def typeVariable : TypeVariable = {
    typeCounter += 1
    TypeVariable(typeCounter.toString)
  }

  private var declarationCounter = 0
  def declarationVariable: DeclarationVariable = {
    declarationCounter += 1
    DeclarationVariable(declarationCounter.toString)
  }
}
