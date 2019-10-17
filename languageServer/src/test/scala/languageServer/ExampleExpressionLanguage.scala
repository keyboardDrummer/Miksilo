package languageServer

import core.language.{FileElement, Language, SourceElementFromFileElement}
import core.parsers.editorParsers.LeftRecursiveCorrectingParserWriter
import core.parsers.strings.{CommonStringReaderParser, WhitespaceParserWriter}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import util.SourceUtils

// TODO add a constraintBuilder that's specific to a File, so you add a [Has]SourceRange instead of a SourceElement
// TODO compute the range based on the children, so only the leafs needs a Range.
trait Expression extends FileElement {
  def collectConstraints(builder: ConstraintBuilder, uri: String, scope: Scope)
}

case class VariableDeclaration(range: SourceRange, name: String) extends FileElement {
  override def childElements = Seq.empty
}

case class Let(range: SourceRange, variable: VariableDeclaration, variableValue: Expression, value: Expression) extends Expression {

  override def collectConstraints(builder: ConstraintBuilder, uri: String, scope: Scope): Unit = {
    val valueScope = builder.newScope(Some(scope))
    builder.declare(variable.name, valueScope, variable.addFile(uri))
    value.collectConstraints(builder, uri, valueScope)
  }

  override def childElements: Seq[FileElement] = Seq(variable, variableValue, value)
}

case class Number(range: SourceRange, value: Int) extends Expression {
  override def collectConstraints(builder: ConstraintBuilder, uri: String, scope: Scope): Unit = {
  }

  override def childElements: Seq[Nothing] = Seq.empty
}

case class Identifier(range: SourceRange, name: String) extends Expression {
  override def collectConstraints(builder: ConstraintBuilder, uri: String, scope: Scope): Unit = {
    builder.resolve(name, scope, this.addFile(uri))
  }

  override def childElements = Seq.empty
}

case class ExpressionHole(range: SourceRange) extends Expression {

  override def collectConstraints(builder: ConstraintBuilder, uri: String, scope: Scope): Unit = {
    builder.refer("", scope, Some(this.addFile(uri)))
  }

  override def childElements = Seq.empty
}

case class Addition(range: SourceRange, left: Expression, right: Expression) extends Expression {
  override def collectConstraints(builder: ConstraintBuilder, uri: String, scope: Scope): Unit = {
    left.collectConstraints(builder, uri, scope)
    right.collectConstraints(builder, uri, scope)
  }

  override def childElements = Seq(left, right)
}

object ExpressionParser extends CommonStringReaderParser with LeftRecursiveCorrectingParserWriter with WhitespaceParserWriter {

  lazy val expression: Self[Expression] = new Lazy(addition | numberParser | let | variable | hole)

  val numberParser: Self[Expression] = wholeNumber.map(x => Integer.parseInt(x)).withSourceRange((range, value) => Number(range, value))

  val addition: Self[Expression] = (expression ~< "+" ~ expression).withSourceRange((range, value) => Addition(range, value._1, value._2))

  val variable: Self[Expression] = parseIdentifier.withSourceRange((range, value) => Identifier(range, value))
  val variableDeclaration = parseIdentifier.withSourceRange((r,x) => VariableDeclaration(r,x))
  val let: Self[Expression] = ("let" ~> variableDeclaration ~< "=" ~ expression ~< "in" ~ expression).withSourceRange((range, t) => Let(range, t._1._1, t._1._2, t._2))
  val hole = Fallback(RegexParser(" *".r, "spaces").withSourceRange((r,_) => ExpressionHole(r)), "expression")

  val root = expression ~< trivias
}

object ExampleExpressionLanguage extends Language {
  private val parsePhase = Language.getParsePhaseFromParser[Expression, _root_.languageServer.ExpressionParser.StringReader](
    stream => {
      val program = SourceUtils.streamToString(stream)
      new ExpressionParser.StringReader(program)
    },
    (program, uri) => SourceElementFromFileElement(uri, program),
    ExpressionParser.expression.getWholeInputParser)

  private val constraintPhase = Language.getConstraintPhase((compilation, builder) => {
    val rootElement = compilation.program.asInstanceOf[SourceElementFromFileElement]
    val rootExpression = rootElement.element.asInstanceOf[Expression]
    rootExpression.collectConstraints(builder, rootElement.uri, builder.newScope(debugName = "rootScope"))
  })

  compilerPhases = List(parsePhase, constraintPhase)
}
