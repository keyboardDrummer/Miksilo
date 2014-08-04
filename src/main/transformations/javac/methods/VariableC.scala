package transformations.javac.methods

import core.transformation._
import transformations.bytecode.instructions.LoadIntegerC
import transformations.javac.base._
import transformations.javac.expressions.ExpressionC


object VariableC extends GrammarTransformation {

  val variableNameKey = "name"

  override def dependencies: Set[Contract] = Set(JavaMethodC, LoadIntegerC)

  override def inject(state: TransformationState): Unit = {
    JavaMethodC.getReferenceKindRegistry(state).put(VariableKey, variable => {
      val methodCompiler = JavaMethodC.getMethodCompiler(state)
      getReferenceKind(variable, methodCompiler)
    })
    ExpressionC.getExpressionToLines(state).put(VariableKey, (variable: MetaObject) => {
      val methodCompiler = JavaMethodC.getMethodCompiler(state)
      val name: String = getVariableName(variable)
      val variableAddress = methodCompiler.variables(name).offset
      Seq(LoadIntegerC.integerLoad(variableAddress))
    })
    ExpressionC.getGetTypeRegistry(state).put(VariableKey, (variable: MetaObject) => {
      val methodCompiler = JavaMethodC.getMethodCompiler(state)
      getType(variable, methodCompiler)
    })
  }

  def getReferenceKind(variable: MetaObject, methodCompiler: MethodCompiler): ReferenceKind = {
    val classCompiler = methodCompiler.classCompiler

    val name = VariableC.getVariableName(variable)
    val isClass = classCompiler.classNames.contains(name)
    if (isClass)
      new ClassOrObjectReference(classCompiler.findClass(name), true)
    else {
      val mbPackage = classCompiler.compiler.env.content.get(name)
      if (mbPackage.isDefined)
        new PackageReference(mbPackage.get.asInstanceOf[PackageInfo])
      else {
        methodCompiler.getReferenceKindFromExpressionType(variable)
      }
    }
  }

  def getVariableName(variable: MetaObject) = variable(variableNameKey).asInstanceOf[String]

  def getType(variable: MetaObject, methodCompiler: MethodCompiler) = {
    methodCompiler.variables(VariableC.getVariableName(variable))._type
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val expression = grammars.find(ExpressionC.ExpressionGrammar)
    val variableGrammar = identifier ^^ (name => variable(name.asInstanceOf[String]))
    expression.inner = expression.inner | variableGrammar
  }

  def variable(name: String) = {
    new MetaObject(VariableKey) {
      data.put(variableNameKey, name)
    }
  }

  object VariableKey

}
