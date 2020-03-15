package core.layouts

import org.apache.commons.math3.linear.{DecompositionSolver, LUDecomposition, MatrixUtils, NonSquareMatrixException}

import scala.util.Try

case class Equation(elements: Map[Variable, Double], constant: Double)


class EquationLayout() {
  private var components = Set.empty[Component]
  val container: Component = createComponent
  var expressions = Set.empty[Expression]

  def createComponent: Component = {
    val result = Component(components.size)
    components += result
    result
  }

  def addEquals(expressions: Expression*): Unit =  {
    for (pair <- expressions.zip(expressions.drop(1))) {
      this.expressions += pair._1 - pair._2
    }
  }

  def addRow(components: Component*): Unit =  {
    addEquals(components.map(c => c.height): _*)
    addEquals(components.map(c => c.bottom): _*)
  }

  def addLeftToRight(components: Component*): Unit =  {
    for (pair <- components.zip(components.drop(1))) {
      expressions += pair._1.right - pair._2.left
    }
  }

  def addTopToBottom(components: Component*): Unit =  {
    for (pair <- components.zip(components.drop(1))) {
      expressions += pair._1.bottom - pair._2.top
    }
  }

  case class TooManyEquationsError(amount: Int, expected: Int) extends RuntimeException
  {
    override def toString = s"too many equations, had $amount, expected $expected"
  }

  def solve(width: Int, height: Int): Map[Variable, Double] = {
    val expressionsWithContainer = expressions ++ Seq(container.left - width, container.right, container.bottom, container.top - height)
    val equations = expressionsWithContainer.map(e => e.toEquation).toSeq

    val expectedVariables = components.flatMap(component => Seq(component.bottom, component.top, component.left, component.right))
    val variables: Set[Variable] = equations.flatMap(equation => equation.elements.keySet).toSet

    val missingExpectedVariables = expectedVariables.diff(variables)
    if (missingExpectedVariables.nonEmpty)
      throw new RuntimeException(s"missing expected variables: $missingExpectedVariables")

    val unexpectedVariables = variables.diff(expectedVariables)
    if (missingExpectedVariables.nonEmpty)
      throw new RuntimeException(s"unexpected variables: $unexpectedVariables")

    val orderedVariables = variables.toSeq
    def elementsToRow(elements: Map[Variable, Double]) = orderedVariables.map(variable => elements.getOrElse(variable, 0.0))
    val a = MatrixUtils.createRealMatrix(equations.map(equation => elementsToRow(equation.elements).toArray).toArray)
    val solver : DecompositionSolver = Try(new LUDecomposition(a).getSolver).
      recover({ case e: NonSquareMatrixException => throw TooManyEquationsError(e.getArgument.intValue() - 4, e.getDimension - 4)}).get
    val b = MatrixUtils.createRealVector(equations.map(equation => -1 * equation.constant).toArray)
    val x = solver.solve(b)
    orderedVariables.zipWithIndex.map(indexedVariable => indexedVariable._1 -> x.getEntry(indexedVariable._2)).toMap
  }
}
