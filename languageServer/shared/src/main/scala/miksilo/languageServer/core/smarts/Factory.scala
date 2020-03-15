package miksilo.languageServer.core.smarts

import miksilo.languageServer.core.smarts.objects.DeclarationVariable
import miksilo.languageServer.core.smarts.scopes.objects.{ConcreteScope, ScopeVariable}
import miksilo.languageServer.core.smarts.types.objects.TypeVariable
import miksilo.languageServer.server.SourcePath

/**
  */
class Factory
{
  private var scopeVariableCounter: Int = 0
  def scopeVariable(): ScopeVariable = {
    scopeVariableCounter += 1
    ScopeVariable(scopeVariableCounter.toString)
  }

  private var scopeCounter: Int = 0
  def newScope(debugName: String = ""): ConcreteScope = {
    scopeCounter += 1
    ConcreteScope(scopeCounter, debugName)
  }

  private var typeCounter = 0
  def typeVariable(origin: Option[SourcePath] = None) : TypeVariable = {
    typeCounter += 1
    TypeVariable(typeCounter.toString, origin)
  }

  private var declarationCounter = 0
  def declarationVariable(): DeclarationVariable = {
    declarationCounter += 1
    DeclarationVariable(declarationCounter.toString)
  }
}
