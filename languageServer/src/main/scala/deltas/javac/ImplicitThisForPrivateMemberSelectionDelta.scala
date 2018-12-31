package deltas.javac

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path._
import core.language.node.Node
import core.language.{Compilation, Language}
import core.smarts.objects.{NamedDeclaration, Reference}
import deltas.expressions.VariableDelta
import deltas.javac.classes.FieldDeclarationDelta.Field
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.classes.skeleton.JavaClassSkeleton.JavaClass
import deltas.javac.classes.{FieldDeclarationDelta, ThisVariableDelta}
import deltas.javac.methods.MethodDelta.Method
import deltas.javac.methods.call._
import deltas.javac.methods.{MemberSelectorDelta, MethodDelta}

object CallVariable extends DeltaWithGrammar {

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val callee = grammars.find(CallDelta.Callee)
    callee.addAlternative(grammars.find(VariableDelta.Shape))
  }

  override def description = "Allow calling a variable as a method."

  override def dependencies = Set(CallDelta, VariableDelta)
}

object ImplicitThisForPrivateMemberSelectionDelta extends DeltaWithPhase {

  override def description: String = "Implicitly prefixes references to private methods with the 'this' qualified if it is missing."

  override def dependencies: Set[Contract] = Set(CallStaticOrInstanceDelta, MethodDelta, JavaClassSkeleton, CallVariable, ThisVariableDelta)

  def addThisToVariable(static: Boolean, clazzName: String, variable: NodeChildPath): Unit = {
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
        val declarationNode = declaration.origin.get.asInstanceOf[FieldPath].parent.current
        declarationNode.shape match {
          case MethodDelta.Shape =>
            val method: Method[Node] = declarationNode
            addThisToVariable(method.isStatic, clazz.name, variable.asInstanceOf[NodeChildPath])
          case FieldDeclarationDelta.Shape =>
            val field: Field[Node] = declarationNode
            addThisToVariable(field.isStatic, clazz.name, variable.asInstanceOf[NodeChildPath])
          case _ =>
        }
      })
    })
  }
}
