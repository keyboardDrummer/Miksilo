package transformations.javac

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import core.particles.path.{Path, PathRoot, FieldValue, SequenceElement}
import transformations.bytecode.ByteCodeSkeleton
import transformations.javac.classes.ClassCompiler
import transformations.javac.classes.skeleton.{ClassSignature, ClassMember, JavaClassSkeleton, JavaCompilerState}
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.methods.call.CallC
import transformations.javac.methods.{MemberSelector, MethodDelta, VariableC}

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
    if (obj.clazz == MethodDelta.MethodKey)
      return new PathRoot(obj.current)

    obj match {
      case FieldValue(parent, field) => FieldValue(getVariableWithCorrectPath(parent), field)
      case SequenceElement(parent, field, index) => SequenceElement(getVariableWithCorrectPath(parent), field, index)
    }
  }

  override def description: String = "Implicitly prefixes references to private methods with the 'this' qualified if it is missing."

  override def transform(program: Node, state: Compilation): Unit = {
    val programWithOrigin = PathRoot(program)
    programWithOrigin.visit(beforeChildren = obj => { obj.clazz match {
            case ByteCodeSkeleton.ClassFileKey =>
              val compiler = JavaCompilerState(state)
              JavaLang.initialise(compiler)
              ClassCompiler(obj, compiler)

            case MethodDelta.MethodKey => MethodDelta.setMethodCompiler(obj, state)
            case VariableC.VariableKey => addThisToVariable(state, obj)
            case _ =>
          }
          true
        })
  }

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val callee = grammars.find(CallC.CallCallee)
    val expression = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    callee.inner = expression
  }
}
