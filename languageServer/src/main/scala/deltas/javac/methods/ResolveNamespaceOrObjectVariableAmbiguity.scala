package deltas.javac.methods

import core.language.node.FieldLocation
import core.smarts.objects.{Declaration, DeclarationVariable, NamedDeclaration}
import core.smarts.types.TypesAreEqual
import core.smarts.types.objects.TypeFromDeclaration
import core.smarts.{Constraint, ConstraintSolver}
import deltas.javac.classes.skeleton.JavaClassSkeleton

case class ResolveNamespaceOrObjectVariableAmbiguity(var namespaceOrObjectVariableDeclaration: Declaration,
                                                     var scopeDeclaration: Declaration) extends Constraint {
  override def apply(solver: ConstraintSolver): Boolean = {
    namespaceOrObjectVariableDeclaration match {
      case e:NamedDeclaration if e.origin.nonEmpty =>
        val value = e.origin.get
        value match {
          case fieldLocation: FieldLocation => fieldLocation.field match {
            case JavaClassSkeleton.Name => //TODO allow referencing packages.
              solver.unifyDeclarations(scopeDeclaration, namespaceOrObjectVariableDeclaration)
            case _ =>
              unifyObjectVariableDeclaration(solver)
          }
          case _ => unifyObjectVariableDeclaration(solver)
        }
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
