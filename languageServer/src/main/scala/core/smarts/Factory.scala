package core.smarts

import core.language.SourceElement
import core.smarts.objects.DeclarationVariable
import core.smarts.scopes.objects.{ConcreteScope, ScopeVariable}
import core.smarts.types.objects.TypeVariable

/**
  */
class Factory
{
  private var scopeVariableCounter: Int = 0
  def scopeVariable(name: Any = null): ScopeVariable = {
    scopeVariableCounter += 1
    ScopeVariable(scopeVariableCounter.toString, name)
  }

  private var scopeCounter: Int = 0
  def newScope(debugName: String = ""): ConcreteScope = {
    scopeCounter += 1
    ConcreteScope(scopeCounter, debugName)
  }

  private var typeCounter = 0
  def typeVariable(origin: Option[SourceElement] = None) : TypeVariable = {
    typeCounter += 1
    TypeVariable(typeCounter.toString, origin)
  }

  private var declarationCounter = 0
  def declarationVariable(debugName: Option[Any] = None): DeclarationVariable = {
    declarationCounter += 1
    DeclarationVariable(declarationCounter.toString, debugName)
  }
}
