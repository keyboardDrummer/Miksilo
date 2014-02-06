package javaInterpreter

class JavaStatement {

}

class While(condition: JavaExpression, body: Seq[JavaStatement]) extends JavaStatement {

}

case class Return(mbValue: Option[JavaExpression]) extends JavaStatement

