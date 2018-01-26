package core.nabl.types

import core.nabl.ConstraintSolver
import core.nabl.objects.{Declaration, DeclarationVariable, NamedDeclaration}
import core.nabl.scopes.{DeclarationNode, DeclaresDeclaration, DeclaresScope, ScopeNode}
import core.nabl.scopes.objects.{Scope, ScopeVariable}
import core.nabl.types.objects.{Type, TypeVariable}

case class InstantiateDeclarationConstraint(var _type: Type, var instantiated: Declaration, var template: Declaration) extends TypeConstraint {
  override def apply(solver: ConstraintSolver): Boolean = template match {
    case named:NamedDeclaration =>
      val scopeGraph = solver.scopeGraph
      val declaredScope = scopeGraph(DeclarationNode(named)).collect({ case x: DeclaresScope => x}).head.target
      val fieldDeclarations = scopeGraph(declaredScope).collect({ case x: DeclaresDeclaration => x})

      def copy(d: NamedDeclaration): NamedDeclaration = new NamedDeclaration(d.name, d.origin)

      val declarationCopy = copy(named)

      val freeVariables: Set[TypeVariable] = fieldDeclarations.flatMap(d => solver.environment(d.target.declaration).variables).toSet
      if (freeVariables.size != 1)
        return false

      val typeParameter = freeVariables.head
      val declaredScopeCopy = ScopeNode(solver.builder.newScope())
      scopeGraph.add(DeclarationNode(declarationCopy), DeclaresScope(declaredScopeCopy))
      fieldDeclarations.foreach(d => {
        val originalDeclaration: NamedDeclaration = d.target.declaration
        val fieldDeclarationCopy: NamedDeclaration = copy(originalDeclaration)
        scopeGraph.add(declaredScopeCopy, DeclaresDeclaration(DeclarationNode(fieldDeclarationCopy)))
        solver.declare(fieldDeclarationCopy, solver.environment(originalDeclaration).instantiateType(typeParameter, _type))
      })

      val result = solver.unifyDeclarations(instantiated, declarationCopy)
      result
    case _ => false
  }

  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
    if (variable == instantiated)
      instantiated = instance
    if (variable == template)
      template = instance
  }

  override def instantiateType(variable: TypeVariable, instance: Type): Unit =  {
    if (variable == _type)
      _type = instance
  }

  override def instantiateScope(variable: ScopeVariable, instance: Scope): Unit = {}

  override def boundTypes: Set[Type] = Set.empty
}
