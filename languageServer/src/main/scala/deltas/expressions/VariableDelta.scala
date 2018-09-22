package deltas.expressions

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.{ChildPath, NodePath}
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.javac.expressions.ExpressionInstance
import deltas.javac.methods.{MethodDelta, VariableInfo}

object VariableDelta extends ExpressionInstance {

  implicit class Variable[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def name: String = node(Name).asInstanceOf[String]
  }

  def neww(name: String) = new Node(Shape, Name -> name)

  object Name extends NodeField

  object Shape extends NodeShape

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val core = find(ExpressionDelta.LastPrecedenceGrammar)
    val variableGrammar = create(Shape, identifier.as(Name) asNode Shape)
    core.addAlternative(variableGrammar)
  }

  override def description: String = "Enables referencing a variable."

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, variable: NodePath, _type: Type, parentScope: Scope): Unit = {
    builder.resolve(variable.name, variable.asInstanceOf[ChildPath], parentScope, Some(_type))
  }

  def getVariableInfo(variable: NodePath, compilation: Compilation): VariableInfo = {
    MethodDelta.getMethodCompiler(compilation).getVariables(variable)(variable.name)
  }

  override def getType(variable: NodePath, compilation: Compilation): Node = {
    getVariableInfo(variable, compilation)._type
  }

  override def shape: NodeShape = Shape
}
