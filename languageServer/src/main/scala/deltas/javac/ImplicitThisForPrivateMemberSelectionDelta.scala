package deltas.javac

import core.deltas._
import core.deltas.path._
import core.language.{Compilation, Language, Phase}
import core.language.node.Node
import core.smarts.SolveConstraintsDelta
import core.smarts.types.objects.TypeFromDeclaration
import deltas.expression.{ExpressionDelta, VariableDelta}
import deltas.javac.classes.skeleton.JavaClassDelta
import deltas.javac.classes.skeleton.JavaClassDelta.JavaClass
import deltas.javac.classes.{FieldDeclarationDelta, ThisVariableDelta}
import deltas.javac.methods.AccessibilityFieldsDelta.HasAccessibility
import deltas.javac.methods.{MemberSelectorDelta, MethodDelta}

object ImplicitThisForPrivateMemberSelectionDelta extends Delta {

  import deltas.HasNameDelta.Name

  import deltas.HasNameDelta.Name

  override def description: String = "Implicitly prefixes references to private methods with the 'this' qualified if it is missing."

  override def dependencies: Set[Contract] = Set(MethodDelta, JavaClassDelta, MemberSelectorDelta, CallVariableDelta, ThisVariableDelta)

  def addThisToVariable(clazz: JavaClass[NodePath], static: Boolean, variable: NodeChildPath): Unit = {

    val newVariableName = if (static) clazz.name else ThisVariableDelta.thisName
    val target = VariableDelta.neww(newVariableName)
    ExpressionDelta.constraintType(target) = TypeFromDeclaration(JavaClassDelta.staticDeclaration(clazz.node))

    val variableNameData = variable.getFieldData(Name)
    val selector = MemberSelectorDelta.Shape.createWithData(
      MemberSelectorDelta.Target -> target,
      MemberSelectorDelta.Member -> variableNameData)

    val variableNode = variable.current
    variableNode.replaceData(selector, keepData = true)
    variableNode.removeField(Name)
  }

  def getVariableWithCorrectPath(path: NodePath): NodePath = {
    path.stopAt(ancestor => ancestor.shape == MethodDelta.Shape)
  }

  override def inject(language: Language): Unit = {
    val phase = Phase(this, compilation => transformProgram(compilation.program, compilation))
    language.insertPhaseAfter(phase, SolveConstraintsDelta)
    super.inject(language)
  }

  def transformProgram(program: Node, compilation: Compilation): Unit = {
    val clazz: JavaClass[NodePath] = PathRoot(program)
    PathRoot(program).visitShape(VariableDelta.Shape, variable =>  {
      val declarationNode: NodePath = SolveConstraintsDelta.getDeclarationOfReference(variable.getSourceElement(Name))
      if (declarationNode.shape == MethodDelta.Shape || declarationNode.shape == FieldDeclarationDelta.Shape) {
        val hasAccessibility: HasAccessibility[NodePath] = new HasAccessibility[NodePath]{ def node = declarationNode }
        addThisToVariable(clazz, hasAccessibility.isStatic, variable.asInstanceOf[NodeChildPath])
      }
    })
  }
}
