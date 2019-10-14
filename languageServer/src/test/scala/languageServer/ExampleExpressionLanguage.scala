package languageServer

import core.language.{FileElement, Language, SourceElementFromFileElement}
import core.parsers.editorParsers.LeftRecursiveCorrectingParserWriter
import core.parsers.strings.{CommonStringReaderParser, WhitespaceParserWriter}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import org.scalatest.FunSuite
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
    builder.resolve(name, this.addFile(uri), scope)
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

  lazy val expression: Self[Expression] = new Lazy(addition | numberParser | let | variable)

  val numberParser: Self[Expression] = wholeNumber.map(x => Integer.parseInt(x)).withSourceRange((range, value) => Number(range, value))

  val addition: Self[Expression] = (expression ~< "+" ~ expression).withSourceRange((range, value) => Addition(range, value._1, value._2))

  val variable: Self[Expression] = parseIdentifier.withSourceRange((range, value) => Identifier(range, value))
  val variableDeclaration = parseIdentifier.withSourceRange((r,x) => VariableDeclaration(r,x))
  val let: Self[Expression] = ("let" ~> variableDeclaration ~< "=" ~ expression ~< "in" ~ expression).withSourceRange((range, t) => Let(range, t._1._1, t._1._2, t._2))

  val root = whiteSpace ~> expression
}

object ExpressionLanguage extends Language {
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

class ExampleExpressionLanguage extends FunSuite with LanguageServerTest {

  val language: Language = ExpressionLanguage
  val program = "let x = 3 in x + 3 + x"
  val server = new MiksiloLanguageServer(language)

  test("parse number") {
    assert(getDiagnostics(server, "3").isEmpty)
  }

  test("parse addition") {
    assert(getDiagnostics(server, "3 + 3").isEmpty)
  }

  test("parse let") {
    assert(getDiagnostics(server, "let x = 3 in x").isEmpty)
  }

  test("go to definition") {
    val firstXUsage = HumanPosition(1, 14)
    val secondXUsage = HumanPosition(1, 22)
    val xDefinition = SourceRange(HumanPosition(1, 5), HumanPosition(1, 6))

    assertResult(xDefinition)(gotoDefinition(server, program, firstXUsage).head.range)
    assertResult(xDefinition)(gotoDefinition(server, program, secondXUsage).head.range)
  }

  test("go to references") {
    val firstXUsage = SourceRange(HumanPosition(1, 14),HumanPosition(1, 15))
    val secondXUsage = SourceRange(HumanPosition(1, 22),HumanPosition(1, 23))
    val xPosition = HumanPosition(1, 5)
    val results = references(server, program, xPosition, includeDeclaration = false)

    assertResult(Seq(firstXUsage,secondXUsage))(results.map(r => r.range))
  }
}
