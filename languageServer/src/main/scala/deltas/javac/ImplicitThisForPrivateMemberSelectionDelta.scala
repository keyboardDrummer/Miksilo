package deltas.javac

import core.deltas._
import core.deltas.path._
import core.language.Compilation
import core.language.node.Node
import core.smarts.SolveConstraintsDelta
import core.smarts.objects.NamedDeclaration
import core.smarts.types.objects.TypeFromDeclaration
import deltas.expression.{ExpressionDelta, VariableDelta}
import deltas.javac.classes.FieldDeclarationDelta.Field
import deltas.javac.classes.skeleton.JavaClassDelta
import deltas.javac.classes.skeleton.JavaClassDelta.JavaClass
import deltas.javac.classes.{FieldDeclarationDelta, ThisVariableDelta}
import deltas.javac.methods.{MemberSelectorDelta, MethodDelta}
import deltas.javac.methods.MethodDelta.Method

object ImplicitThisForPrivateMemberSelectionDelta extends DeltaWithPhase {

  override def description: String = "Implicitly prefixes references to private methods with the 'this' qualified if it is missing."

  override def dependencies: Set[Contract] = Set(MethodDelta, JavaClassDelta, CallVariableDelta, ThisVariableDelta)

  def addThisToVariable(compilation: Compilation, clazz: JavaClass[NodePath],
                        static: Boolean, variable: NodeChildPath): Unit = {

    val newVariableName = if (static) clazz.name else ThisVariableDelta.thisName
    val target = VariableDelta.neww(newVariableName)
    ExpressionDelta.nodeType(target) = TypeFromDeclaration(JavaClassDelta.staticDeclaration(clazz.node))

    val variableNameData = variable.getFieldData(VariableDelta.Name)
    val selector = MemberSelectorDelta.Shape.createWithData(
      MemberSelectorDelta.Target -> target,
      MemberSelectorDelta.Member -> variableNameData)

    val variableNode = variable.current
    variableNode.replaceData(selector, keepData = true)
    variableNode.removeField(VariableDelta.Name)
  }

  def getVariableWithCorrectPath(path: NodePath): NodePath = {
    path.stopAt(ancestor => ancestor.shape == MethodDelta.Shape)
  }

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val clazz: JavaClass[NodePath] = PathRoot(program)
    val thisDeclaration = compilation.proofs.resolveDeclaration(ThisVariableDelta.thisDeclarationField(clazz.node)).asInstanceOf[NamedDeclaration]
    val clazzDeclaration = compilation.proofs.scopeGraph.elementToNode(clazz.node.getSourceElement(JavaClassDelta.Name)).asInstanceOf[NamedDeclaration]
    PathRoot(program).visitShape(VariableDelta.Shape, variable =>  {
      val maybeDeclaration: Option[NamedDeclaration] = SolveConstraintsDelta.resolvesToDeclaration.get(variable.getSourceElement(VariableDelta.Name)).
        map(d => compilation.proofs.resolveDeclaration(d).asInstanceOf[NamedDeclaration])
      maybeDeclaration.foreach(declaration => {
        val declarationNode = declaration.origin.get.asInstanceOf[FieldPath].parent.current
        declarationNode.shape match {
          case MethodDelta.Shape =>
            val method: Method[Node] = declarationNode
            addThisToVariable(compilation, clazz, method.isStatic, variable.asInstanceOf[NodeChildPath])
          case FieldDeclarationDelta.Shape =>
            val field: Field[Node] = declarationNode
            addThisToVariable(compilation, clazz, field.isStatic, variable.asInstanceOf[NodeChildPath])
          case _ =>
        }
      })
    })
  }
}
