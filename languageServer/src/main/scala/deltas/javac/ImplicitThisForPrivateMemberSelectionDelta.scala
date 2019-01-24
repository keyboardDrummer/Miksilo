package deltas.javac

import core.deltas._
import core.deltas.path._
import core.language.Compilation
import core.language.node.Node
import core.smarts.objects.{NamedDeclaration, Reference}
import deltas.expression.VariableDelta
import deltas.javac.classes.FieldDeclarationDelta.Field
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.classes.skeleton.JavaClassSkeleton.JavaClass
import deltas.javac.classes.{FieldDeclarationDelta, ThisVariableDelta}
import deltas.javac.methods.MethodDelta.Method
import deltas.javac.methods.{HasScopeSkeleton, MemberSelectorDelta, MethodDelta}

object ImplicitThisForPrivateMemberSelectionDelta extends DeltaWithPhase {

  override def description: String = "Implicitly prefixes references to private methods with the 'this' qualified if it is missing."

  override def dependencies: Set[Contract] = Set(MethodDelta, JavaClassSkeleton, CallVariableDelta, ThisVariableDelta)

  def addThisToVariable(static: Boolean, classDeclaration: NamedDeclaration, variable: NodeChildPath): Unit = {
    val newVariableName = if (static) classDeclaration.name else ThisVariableDelta.thisName
    val target = VariableDelta.neww(newVariableName)
    HasScopeSkeleton.scopeDeclaration(target) = classDeclaration
    val selector = MemberSelectorDelta.Shape.createWithSource(
      MemberSelectorDelta.Target -> target,
      MemberSelectorDelta.Member -> variable.getWithSource(VariableDelta.Name))
    variable.replaceWith(selector)
  }

  def getVariableWithCorrectPath(path: NodePath): NodePath = {
    path.stopAt(ancestor => ancestor.shape == MethodDelta.Shape)
  }

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val clazz: JavaClass[NodePath] = PathRoot(program)
    val clazzDeclaration = compilation.proofs.scopeGraph.elementToNode(clazz.node.getSourceElement(JavaClassSkeleton.Name)).asInstanceOf[NamedDeclaration]
    PathRoot(program).visitShape(VariableDelta.Shape, variable =>  {
      val maybeGraphNode = compilation.proofs.scopeGraph.elementToNode.get(variable)
      val reference: Reference = maybeGraphNode.get.asInstanceOf[Reference]
      val maybeDeclaration: Option[NamedDeclaration] = compilation.proofs.declarations.get(reference)
      maybeDeclaration.foreach(declaration => {
        val declarationNode = declaration.origin.get.asInstanceOf[FieldPath].parent.current
        declarationNode.shape match {
          case MethodDelta.Shape =>
            val method: Method[Node] = declarationNode
            addThisToVariable(method.isStatic, clazzDeclaration, variable.asInstanceOf[NodeChildPath])
          case FieldDeclarationDelta.Shape =>
            val field: Field[Node] = declarationNode
            addThisToVariable(field.isStatic, clazzDeclaration, variable.asInstanceOf[NodeChildPath])
          case _ =>
        }
      })
    })
  }
}
