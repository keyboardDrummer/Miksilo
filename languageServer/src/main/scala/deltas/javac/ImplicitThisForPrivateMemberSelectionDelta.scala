package deltas.javac

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path._
import core.language.node.{FieldLocation, Node}
import core.language.{Compilation, Language}
import core.smarts.objects.{NamedDeclaration, Reference}
import deltas.expressions.{ExpressionDelta, VariableDelta}
import deltas.javac.classes.FieldDeclarationDelta.Field
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.classes.skeleton.JavaClassSkeleton.JavaClass
import deltas.javac.classes.{FieldDeclarationDelta, ThisVariableDelta}
import deltas.javac.methods.MethodDelta.Method
import deltas.javac.methods.call.{CallDelta, CallStaticOrInstanceDelta}
import deltas.javac.methods.{MemberSelectorDelta, MethodDelta}

object ImplicitThisForPrivateMemberSelectionDelta extends DeltaWithPhase with DeltaWithGrammar {

  override def description: String = "Implicitly prefixes references to private methods with the 'this' qualified if it is missing."

  override def dependencies: Set[Contract] = Set(CallStaticOrInstanceDelta, MethodDelta, JavaClassSkeleton, ThisVariableDelta)

  def addThisToVariable(static: Boolean, clazzName: String, variable: ChildPath): Unit = {
    val newVariableName = if (static) clazzName else ThisVariableDelta.thisName
    val selector = MemberSelectorDelta.Shape.createWithSource(
      MemberSelectorDelta.Target -> VariableDelta.neww(newVariableName),
      MemberSelectorDelta.Member -> variable.getWithSource(VariableDelta.Name))
    variable.replaceWith(selector)
  }

  def getVariableWithCorrectPath(path: NodePath): NodePath = {
    path.stopAt(ancestor => ancestor.shape == MethodDelta.Shape)
  }

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val clazz: JavaClass[Node] = program
    PathRoot(program).visitShape(VariableDelta.Shape, variable =>  {
      val maybeGraphNode = compilation.proofs.scopeGraph.elementToNode.get(variable)
      val reference: Reference = maybeGraphNode.get.asInstanceOf[Reference]
      val maybeDeclaration: Option[NamedDeclaration] = compilation.proofs.declarations.get(reference)
      maybeDeclaration.foreach(declaration => {
        val declarationNode = declaration.origin.get.asInstanceOf[FieldLocation].node
        declarationNode.shape match {
          case MethodDelta.Shape =>
            val method: Method[Node] = declarationNode
            addThisToVariable(method.isStatic, clazz.name, variable.asInstanceOf[ChildPath])
          case FieldDeclarationDelta.Shape =>
            val field: Field[Node] = declarationNode
            addThisToVariable(field.isStatic, clazz.name, variable.asInstanceOf[ChildPath])
          case _ =>
        }
      })
    })
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val callee = grammars.find(CallDelta.Callee)
    val expression = grammars.find(ExpressionDelta.FirstPrecedenceGrammar)
    callee.inner = expression
  }
}
