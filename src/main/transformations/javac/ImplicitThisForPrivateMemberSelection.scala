package transformations.javac

import core.particles._
import transformations.javac.classes.JavaClassSkeleton
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.methods.{MemberSelector, MethodC, VariableC}

import scala.collection.mutable

/* TODO: the implementation for this class is rather extreme. It's probably slow as well.
// You can't do one big transformation here because when transforming a variable you need the complete scope.
// Currently I've just shoved the transformation in front of all calls to getType and toInstructions, which works but seems dangerous.
// I think I'd prefer just going over the program once, with all the scope information, and transforming all the variables.
// Currently LocalDeclaration adds scoping information in its toByteCode method which is a bit unexpected!
*/ 
object ImplicitThisForPrivateMemberSelection extends Particle {
  val thisName: String = "this"

  override def dependencies: Set[Contract] = Set(MethodC, JavaClassSkeleton)

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    val visited = new mutable.HashSet[MetaObject]()
    transformToByteCodeInstructions(state, visited)
    transformGetType(state, visited)
  }

  def transformGetType(state: CompilationState, visited: mutable.HashSet[MetaObject]): Unit = {
    val registry = ExpressionSkeleton.getState(state).getTypeRegistry
    val transformed = registry.mapValues(original => (root: MetaObject) => {
      root.transform(visited, obj => transformMetaObject(state)(obj))
      original(root)
    }).toSeq
    registry.clear()
    registry ++= transformed
  }

  def transformToByteCodeInstructions(state: CompilationState, visited: mutable.HashSet[MetaObject]): Unit = {
    val registry = ExpressionSkeleton.getToInstructionsRegistry(state)
    val transformed = registry.mapValues(original => (root: MetaObject) => {
      root.transform(visited, obj => transformMetaObject(state)(obj))
      original(root)
    }).toSeq
    registry.clear()
    registry ++= transformed
  }

  def transformMetaObject(state: CompilationState): (MetaObject) => Unit = {
    metaObject => metaObject.clazz match {
      case VariableC.VariableKey => addThisToVariable(state, metaObject)
      case _ =>
    }
  }

  def addThisToVariable(state: CompilationState, variable: MetaObject) {
    val compiler = JavaClassSkeleton.getClassCompiler(state)
    val methodCompiler = MethodC.getMethodCompiler(state)

    val name = VariableC.getVariableName(variable)
    if (!methodCompiler.variables.contains(name)) {
      val currentClass = compiler.currentClassInfo
      currentClass.content.get(name).foreach(classMember => {
        val newVariableName = if (classMember._static) currentClass.name else thisName
        val selector = MemberSelector.selector(VariableC.variable(newVariableName), name)
        variable.replaceWith(selector)
      })
    }
  }

  override def description: String = "Implicitly prefixes references to private methods with the 'this' qualified if it is missing."
}
