package typed.languages.javaInterpreter

class JavaExpression extends JavaStatement {

}

case class Call(callee: JavaExpression, arguments: Seq[JavaExpression]) extends JavaExpression

case class Ternary(condition: JavaExpression, trueResult: JavaExpression, falseResult: JavaExpression) extends JavaExpression

case class Literal(value: AnyVal) extends JavaExpression

case class Selection(_object: JavaExpression, member: String) extends JavaExpression

case class JavaEquals(first: JavaExpression, second: JavaExpression) extends JavaExpression
case class LessThan(first: JavaExpression, second: JavaExpression) extends JavaExpression
case class Variable(name: String) extends JavaExpression
case class Addition(first: JavaExpression, second: JavaExpression) extends JavaExpression
case class JavaSubtraction(first: JavaExpression, second: JavaExpression) extends JavaExpression