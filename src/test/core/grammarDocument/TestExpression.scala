package core.grammarDocument

case class Value(value: Int) extends TestExpression {
  override def compute = value
}

trait TestExpression {
  def compute: Int
}

case class Add(first: TestExpression, second: TestExpression) extends TestExpression {
  override def compute = first.compute + second.compute
}

case class Multiply(first: TestExpression, second: TestExpression) extends TestExpression {
  override def compute = first.compute * second.compute
}

