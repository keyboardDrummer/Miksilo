package deltas.javac

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path._
import core.language.node.Node
import core.language.{Compilation, Language}
import deltas.expressions.{ExpressionDelta, VariableDelta}
import deltas.javac.classes.skeleton.JavaClassSkeleton.getState
import deltas.javac.classes.skeleton.{ClassMember, ClassSignature, JavaClassSkeleton}
import deltas.javac.classes.{ClassCompiler, ThisVariableDelta}
import deltas.javac.methods.call.CallDelta
import deltas.javac.methods.{MemberSelectorDelta, MethodDelta, VariableToByteCodeDelta}

object ImplicitThisForPrivateMemberSelection extends DeltaWithPhase with DeltaWithGrammar {

  override def description: String = "Implicitly prefixes references to private methods with the 'this' qualified if it is missing."

  override def dependencies: Set[Contract] = Set(MethodDelta, JavaClassSkeleton, ThisVariableDelta)

  def addThisToVariable(compilation: Compilation, variable: ChildPath) {
    val compiler = JavaClassSkeleton.getClassCompiler(compilation)

    val name = VariableDelta.getVariableName(variable)
    val variableWithCorrectPath: NodePath = getVariableWithCorrectPath(variable)
    if (!MethodDelta.getMethodCompiler(compilation).getVariables(variableWithCorrectPath).contains(name)) {
      val currentClass = compiler.currentClassInfo
      currentClass.methods.keys.find(key => key.methodName == name).foreach(key => {
        val classMember: ClassMember = currentClass.methods(key)
        addThisToVariable(classMember, currentClass, variable)
      })

      currentClass.fields.keys.find(key => key == name).foreach(key => {
        val classMember = currentClass.fields(key)
        addThisToVariable(classMember, currentClass, variable)
      })
    }
  }

  def addThisToVariable(classMember: ClassMember, currentClass: ClassSignature, variable: ChildPath): Unit = {
    val newVariableName = if (classMember._static) currentClass.name else ThisVariableDelta.thisName
    val selector = MemberSelectorDelta.Shape.createWithSource(
      MemberSelectorDelta.Target -> VariableDelta.neww(newVariableName),
      MemberSelectorDelta.Member -> variable.getWithSource(VariableDelta.Name))
    variable.replaceWith(selector)
  }

  def getVariableWithCorrectPath(obj: NodePath): NodePath = {
    if (obj.shape == MethodDelta.Shape)
      return PathRoot(obj.current)

    obj match {
      case FieldValue(parent, field) => FieldValue(getVariableWithCorrectPath(parent), field)
      case SequenceElement(parent, field, index) => SequenceElement(getVariableWithCorrectPath(parent), field, index)
    }
  }

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val programWithOrigin: NodePath = PathRoot(program)
    programWithOrigin.visit(beforeChildren = obj => { obj.shape match {
        case JavaClassSkeleton.Shape =>
          JavaLang.loadIntoClassPath(compilation)

          val classCompiler = ClassCompiler(obj, compilation)
          getState(compilation).classCompiler = classCompiler
          classCompiler.bind()

        case MethodDelta.Shape => MethodDelta.setMethodCompiler(obj, compilation)
        case VariableDelta.Shape => addThisToVariable(compilation, obj.asInstanceOf[ChildPath])
        case _ =>
      }
      true
    })
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val callee = grammars.find(CallDelta.Callee)
    val expression = grammars.find(ExpressionDelta.FirstPrecedenceGrammar)
    callee.inner = expression
  }
}
