package core.layouts

object Expression
{
  implicit def constant(value: Double) = new Constant(value)
}

trait Expression {
  def +(right: Expression) = new Add(this, right)

  def *(right: Double) = new Multiply(this, right)

  def -(right: Expression) = this + (right * -1)

  def getElements: Map[Variable, Double]

  def getConstant: Double

  def toEquation = new Equation(getElements, getConstant)
}

case class Add(left: Expression, right: Expression) extends Expression {
  override def getElements: Map[Variable, Double] = {
    val leftElements = left.getElements
    val rightElements = right.getElements
    val entries = for {
      key <- leftElements.keySet ++ rightElements.keySet
      left = leftElements.getOrElse(key, 0.0)
      right = rightElements.getOrElse(key, 0.0)
    } yield (key, left + right)
    entries.toMap
  }

  override def getConstant: Double = left.getConstant + right.getConstant

  override def toString = s"$left + $right"
}

case class Constant(value: Double) extends Expression {
  override def getElements: Map[Variable, Double] = Map.empty

  override def getConstant: Double = value

  override def toString = value.toString
}

case class Multiply(left: Expression, right: Double) extends Expression {
  override def getElements: Map[Variable, Double] = left.getElements.mapValues(v => v * right)

  override def getConstant: Double = left.getConstant * right

  override def toString = s"left * right"
}

case class Variable(component: Component, property: ComponentProperty) extends Expression {
  override def getElements: Map[Variable, Double] = Map(this -> 1)

  override def getConstant: Double = 0

  override def toString = s"var(${component.index},$property)"
}