package miksilo.languageServer.server

import miksilo.languageServer.core.language.{FileElement, Language, SourcePathFromElement}
import miksilo.editorParser.parsers.editorParsers.{LeftRecursiveCorrectingParserWriter, OffsetPointerRange}
import miksilo.editorParser.parsers.strings.{CommonStringReaderParser, WhitespaceParserWriter}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope

// TODO add a constraintBuilder that's specific to a File, so you add a SourceElement instead of a SourcePath
// TODO compute the range based on the children, so only the leafs needs a Range.

trait Expression extends FileElement {
  def collectConstraints(builder: ConstraintBuilder, uri: String, scope: Scope): Unit
}

// Syntax: let x = 3 in x + 2
case class Let(range: OffsetPointerRange,
               variable: VariableDeclaration, // let _x_
               variableValue: Expression, // 3
               value: Expression // x + 2
              )
  extends Expression {

  override def collectConstraints(builder: ConstraintBuilder, uri: String, scope: Scope): Unit = {
    val valueScope = builder.newScope(scope)
    builder.declare(variable.name, valueScope, variable.addFile(uri))
    value.collectConstraints(builder, uri, valueScope)
  }

  override def childElements: Seq[FileElement] = Seq(variable, variableValue, value)
}

// Syntax: some_identifier_name
case class Identifier(range: OffsetPointerRange, name: String) extends Expression {
  override def collectConstraints(builder: ConstraintBuilder, uri: String, scope: Scope): Unit = {
    builder.resolve(name, scope, this.addFile(uri))
  }

  override def childElements = Seq.empty
}

case class VariableDeclaration(range: OffsetPointerRange, name: String) extends FileElement {
  override def childElements = Seq.empty
}

case class Number(range: OffsetPointerRange, value: Int) extends Expression {
  override def collectConstraints(builder: ConstraintBuilder, uri: String, scope: Scope): Unit = {
  }

  override def childElements: Seq[Nothing] = Seq.empty
}

/** Occurs in places where an expression is missing, like `3 + <here>`
  * In that case, provide code completion for variables
  */
case class ExpressionHole(range: OffsetPointerRange) extends Expression {

  override def collectConstraints(builder: ConstraintBuilder, uri: String, scope: Scope): Unit = {
    builder.refer("", scope, Some(this.addFile(uri)))
  }

  override def childElements = Seq.empty
}

// Syntax: 3 + 2
case class Addition(range: OffsetPointerRange, left: Expression, right: Expression) extends Expression {
  override def collectConstraints(builder: ConstraintBuilder, uri: String, scope: Scope): Unit = {
    left.collectConstraints(builder, uri, scope)
    right.collectConstraints(builder, uri, scope)
  }

  override def childElements = Seq(left, right)
}

object ExpressionParser extends CommonStringReaderParser with LeftRecursiveCorrectingParserWriter with WhitespaceParserWriter {

  lazy val expression: Parser[Expression] = new Lazy(addition | numberParser | let |
    variable | hole)

  val numberParser: Parser[Expression] = wholeNumber.map(x => Integer.parseInt(x)).
    withSourceRange((range, value) => Number(range, value))

  val addition: Parser[Expression] = (expression ~< "+" ~ expression).
    withSourceRange((range, operands) => Addition(range, operands._1, operands._2))

  val variable: Parser[Expression] = parseIdentifier.
    withSourceRange((range, name) => Identifier(range, name))

  val variableDeclaration = parseIdentifier.
    withSourceRange((range, name) => VariableDeclaration(range, name))

  val let: Parser[Expression] = ("let" ~> variableDeclaration ~< "=" ~ expression ~< "in" ~ expression).
    withSourceRange((range, args) => Let(range, args._1._1, args._1._2, args._2))

  val hole = Fallback(RegexParser(" *".r, "spaces").
    withSourceRange((range, _) => ExpressionHole(range)), "expression")

  val root = expression ~< trivias
}

object ExampleExpressionLanguage extends Language {
  private val parsePhase = Language.getCachingParsePhase[Expression](
    (program, uri) => SourcePathFromElement(uri, program),
    ExpressionParser.expression.getWholeInputParser(), indentationSensitive = false)

  private val constraintPhase = Language.getConstraintPhase((compilation, builder) => {
    val rootElement = compilation.program.asInstanceOf[SourcePathFromElement]
    val rootExpression = rootElement.sourceElement.asInstanceOf[Expression]
    rootExpression.collectConstraints(builder, rootElement.uri, builder.newScope(debugName = "rootScope"))
  })

  compilerPhases = List(parsePhase, constraintPhase)
}
