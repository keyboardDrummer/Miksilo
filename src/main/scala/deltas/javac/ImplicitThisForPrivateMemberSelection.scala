package deltas.javac

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.Node
import core.deltas.path.{FieldValue, Path, PathRoot, SequenceElement}
import deltas.bytecode.ByteCodeSkeleton
import deltas.javac.classes.ClassCompiler
import deltas.javac.classes.skeleton.JavaClassSkeleton.getState
import deltas.javac.classes.skeleton.{ClassMember, ClassSignature, JavaClassSkeleton}
import deltas.javac.expressions.ExpressionSkeleton
import deltas.javac.methods.call.CallC
import deltas.javac.methods.{MemberSelector, MethodDelta, VariableC}

object ImplicitThisForPrivateMemberSelection extends DeltaWithPhase with DeltaWithGrammar {
  val thisName: String = "this"

  override def dependencies: Set[Contract] = Set(MethodDelta, JavaClassSkeleton)

  def addThisToVariable(compilation: Compilation, variable: Path) {
    val compiler = JavaClassSkeleton.getClassCompiler(compilation)

    val name = VariableC.getVariableName(variable)
    val variableWithCorrectPath: Path = getVariableWithCorrectPath(variable)
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

  def addThisToVariable(classMember: ClassMember, currentClass: ClassSignature, variable: Path) = {
    val name = VariableC.getVariableName(variable)
    val newVariableName = if (classMember._static) currentClass.name else thisName
    val selector = MemberSelector.selector(VariableC.variable(newVariableName), name)
    variable.replaceWith(selector)
  }

  def getVariableWithCorrectPath(obj: Path): Path = {
    if (obj.clazz == MethodDelta.Clazz)
      return PathRoot(obj.current)

    obj match {
      case FieldValue(parent, field) => FieldValue(getVariableWithCorrectPath(parent), field)
      case SequenceElement(parent, field, index) => SequenceElement(getVariableWithCorrectPath(parent), field, index)
    }
  }

  override def description: String = "Implicitly prefixes references to private methods with the 'this' qualified if it is missing."

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val programWithOrigin = PathRoot(program)
    programWithOrigin.visit(beforeChildren = obj => { obj.clazz match {
            case ByteCodeSkeleton.Clazz =>
              JavaLang.loadIntoClassPath(compilation)

              val classCompiler = ClassCompiler(obj, compilation)
              getState(compilation).classCompiler = classCompiler
              classCompiler.bind()

            case MethodDelta.Clazz => MethodDelta.setMethodCompiler(obj, compilation)
            case VariableC.VariableKey => addThisToVariable(compilation, obj)
            case _ =>
          }
          true
        })
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val callee = grammars.find(CallC.CallCallee)
    val expression = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    callee.inner = expression
  }
}
