package transformations.javac

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.path.{Root, SequenceSelection, Selection}
import transformations.bytecode.ByteCodeSkeleton
import transformations.javac.classes.JavaClassSkeleton
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.methods.{CallC, MemberSelector, MethodC, VariableC}

object ImplicitThisForPrivateMemberSelection extends ParticleWithPhase with ParticleWithGrammar {
  val thisName: String = "this"

  override def dependencies: Set[Contract] = Set(MethodC, JavaClassSkeleton)

  def addThisToVariable(state: CompilationState, variable: Path) {
    val compiler = JavaClassSkeleton.getClassCompiler(state)

    val name = VariableC.getVariableName(variable)
    val variableWithCorrectPath: Path = getVariableWithCorrectPath(variable)
    if (!VariableC.getVariables(state, variableWithCorrectPath).contains(name)) {
      val currentClass = compiler.currentClassInfo
      currentClass.content.get(name).foreach(classMember => {
        val newVariableName = if (classMember._static) currentClass.name else thisName
        val selector = MemberSelector.selector(VariableC.variable(newVariableName), name)
        variable.replaceWith(selector)
      })
    }
  }

  def getVariableWithCorrectPath(obj: Path): Path = {
    if (obj.clazz == MethodC.MethodKey)
      return new Root(obj.current)

    obj match {
      case Selection(parent, field) => Selection(getVariableWithCorrectPath(parent), field)
      case SequenceSelection(parent, field, index) => SequenceSelection(getVariableWithCorrectPath(parent), field, index)
    }
  }

  override def description: String = "Implicitly prefixes references to private methods with the 'this' qualified if it is missing."

  override def transform(program: MetaObject, state: CompilationState): Unit = {
    val programWithOrigin = new Root(program)
    programWithOrigin.transform[Path](obj => obj.clazz match {
      case ByteCodeSkeleton.ClassFileKey => JavaClassSkeleton.initializeClassCompiler(state, program)
      case MethodC.MethodKey => MethodC.setMethodCompiler(obj, state)
      case VariableC.VariableKey => addThisToVariable(state, obj)
      case _ =>
    })
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val callee = grammars.find(CallC.CallCallee)
    val expression = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    callee.inner = expression
  }
}
