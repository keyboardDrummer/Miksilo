package deltas.javac.methods

import core.language.SourceElement
import core.language.node.Node
import core.smarts.objects.{Declaration, DeclarationVariable, NamedDeclaration}
import core.smarts.types.TypesAreEqual
import core.smarts.types.objects.TypeFromDeclaration
import core.smarts.{Constraint, ConstraintSolver}
import deltas.javac.classes.skeleton.JavaClassSkeleton

case class GetScopeDeclarationForNamespaceOrObjectVariableDeclaration(var namespaceOrObjectVariableDeclaration: Declaration,
                                                                      var scopeDeclaration: Declaration) extends Constraint {
  override def apply(solver: ConstraintSolver): Boolean = {
    namespaceOrObjectVariableDeclaration match {
      case e:NamedDeclaration =>
        val path = e.origin.asInstanceOf[SourceElement]
        val value = path.current
        value match {
          case node: Node => node.shape match {
            case JavaClassSkeleton.Shape => //TODO allow referencing packages.
              solver.unifyDeclarations(scopeDeclaration, namespaceOrObjectVariableDeclaration)
            case _ =>
              unifyObjectVariableDeclaration(solver)
          }
          case _ => unifyObjectVariableDeclaration(solver)
        } //TODO de order of the scopes matters here because the builder hasn't moved the new constraints into solver.constraints yet. Fix
        true
      case _ => false
    }
  }

  private def unifyObjectVariableDeclaration(solver: ConstraintSolver) = {
    val objectType = solver.builder.getType(namespaceOrObjectVariableDeclaration)
    solver.builder.add(TypesAreEqual(TypeFromDeclaration(scopeDeclaration), objectType))
  }

  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
    if (namespaceOrObjectVariableDeclaration == variable)
      namespaceOrObjectVariableDeclaration = instance
    if (scopeDeclaration == variable)
      scopeDeclaration = instance
    super.instantiateDeclaration(variable, instance)
  }
}
