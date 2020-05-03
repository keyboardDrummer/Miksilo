
package miksilo.modularLanguages.deltas.yaml

import miksilo.editorParser.SourceUtils
import miksilo.editorParser.parsers.editorParsers.UntilBestAndXStepsStopFunction
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.bigrammar.SelectGrammar
import miksilo.modularLanguages.core.deltas.path.{ChildPath, PathRoot}
import miksilo.modularLanguages.deltas.expression.{ArrayLiteralDelta, ExpressionDelta, StringLiteralDelta}
import miksilo.modularLanguages.deltas.json.JsonObjectLiteralDelta
import miksilo.modularLanguages.util.TestLanguageBuilder
import org.scalatest.funsuite.AnyFunSuite

class ModularYamlTest extends AnyFunSuite {

  val language = TestLanguageBuilder.buildWithParser(ModularYamlLanguage.deltasWithoutParser,
    stopFunction = UntilBestAndXStepsStopFunction(), indentationSensitive = true)

  test("plain scalar in object value") {
    val program = """{a: b}"""
    val compilation = language.compileString(program)
    assert(compilation.diagnostics.isEmpty)
  }

  test("explicit document") {
    val program =
      """---
        |Joo
        |""".stripMargin
    val compilation = language.compileString(program)
    assert(compilation.diagnostics.isEmpty)
  }

  test("literal scalar") {
    val program = "|+\n     Joo\n     Joo2"
    val compilation = language.compileString(program)
    assert(compilation.diagnostics.isEmpty)
  }

  test("literal scalar 2") {
    val program = "|+\n    {\n     \"Key\":\"Value\"\n    }"
    val compilation = language.compileString(program)
    assert(compilation.diagnostics.isEmpty)
  }

  test("nested literal scalar") {
    val program =
      """
- |-
    {
      "Key": "Value"
    }
- {
    "AnotherKey": "AnotherValue"
  }"""
    val compilation = language.compileString(program)
    assert(compilation.diagnostics.isEmpty)
  }

  test("plain scalar with -") {
    val program = """AllowedPattern: ([a-zA-Z0-9_\-\.]+)@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.)|(([a-zA-Z0-9\-]+\.)+))([a-zA-Z]{2,4}|[0-9]{1,3})(\]?)"""
    val compilation = language.compileString(program)
    assert(compilation.diagnostics.isEmpty)
  }

  test("regression") {
    val program =
      """- Bar:
        |   Joo
        |- 2""".stripMargin
    val compilation = language.compileString(program)
    assert(compilation.diagnostics.isEmpty)
  }

  test("regression 2") {
    val program = "- Bar:\n   \n- 2"
    val compilation = language.compileString(program)
    // TODO fix plain scalar so these asserts can be swapped.
    assert(compilation.diagnostics.isEmpty)
    // assert(compilation.diagnostics.size == 1 && compilation.diagnostics.head.diagnostic.message.contains("expected '<value>'"))
  }

  test("regression 3") {
    val program = """{
                    |  "Parameters" : {
                    |
                    |    "BrokenParameter",
                    |    "VpcId" : 3
                    |  }
                    |}""".stripMargin
    val compilation = language.compileString(program)
    assert(compilation.diagnostics.size == 1 && compilation.diagnostics.head.diagnostic.message.contains("expected ':<value>'"))
  }

  test("error case") {
    val program = """Foo: bar
                    | yoo: hee""".stripMargin
    val compilation = language.compileString(program)
    assert(compilation.diagnostics.size == 2 && compilation.diagnostics.head.diagnostic.message.contains(":{"))
  }

  test("array as key") {
    val program = """Foo: [bar,
                    | yoo]: hee""".stripMargin
    val compilation = language.compileString(program)
    assert(compilation.diagnostics.isEmpty)
  }

  test("compact array") {
    val input = """SecurityGroupIngress:
                  |- IpProtocol: tcp
                  |  FromPort: 22""".stripMargin

    val compilation = language.compileString(input)
    assert(compilation.diagnostics.isEmpty)
    val array = JsonObjectLiteralDelta.ObjectLiteral(compilation.program.asInstanceOf[PathRoot].current).members.head.value
    val arrayMembers = ArrayLiteralDelta.ArrayLiteral(array).members
    val nestedObject = JsonObjectLiteralDelta.ObjectLiteral(arrayMembers.head)
    assert(nestedObject.members.length == 2)
  }

  test("compact array with negative indentation") {
    val input = """ SecurityGroupIngress:
                  |- IpProtocol: tcp
                  |  FromPort: 22""".stripMargin

    val compilation = language.compileString(input)
    assert(compilation.diagnostics.nonEmpty)
  }

  test("single member object without a value") {
    val program = "Key:"
    val compilation = language.compileString(program)
    replaceDefaultWithDefaultString(compilation)

    val reference = "Key:'default'"
    val referenceCompilation = language.compileString(reference)
    assertResult(referenceCompilation.program)(compilation.program)
    assert(compilation.diagnostics.size == 1)
  }

  val twoMemberObject =
    """Missing: default
      |Key: Value
    """.stripMargin
  lazy val twoMemberObjectCompilation = language.compileString(twoMemberObject)

  test("two member object with no first value") {
    val program =
      """Missing:
        |Key: Value""".stripMargin
    val compilation = language.compileString(program)
    replaceDefaultWithDefaultString(compilation)

    val reference =
      """Missing: default
        |Key: Value
      """.stripMargin
    val referenceCompilation = language.compileString(reference)
    assertResult(referenceCompilation.program)(compilation.program)
    assert(compilation.diagnostics.size == 1)
  }

  test("two member object with no first value and colon") {
    val program =
      """Missing
        |Key: Value
      """.stripMargin
    val compilation = language.compileString(program)
    replaceDefaultWithDefaultString(compilation)

    assertResult(twoMemberObjectCompilation.program)(compilation.program)
    assert(compilation.diagnostics.size == 1)
  }


  val twoObjectsSingleMemberEach =
    """Parent1:
      |  HasValue: Value Value Value
      |  MissingValue: default
      |Parent2:
      |  HasValue2: Value2
    """.stripMargin
  lazy val twoObjectsSingleMemberEachCompilation = language.compileString(twoObjectsSingleMemberEach)

  test("complicated middle errors") {
    val program =
      """Parent1:
        |  HasValue: Value Value Value
        |  MissingValue
        |Parent2:
        |  HasValue2: Value2
      """.stripMargin
    val compilation = language.compileString(program)

    replaceDefaultWithDefaultString(compilation)
    assertResult(twoObjectsSingleMemberEachCompilation.program)(compilation.program)
    assert(compilation.diagnostics.size == 1)
  }

  test("Broken in the middle") {
    val program =
      """Parameters:
        |  KeyName: EC2
        |  MemberWithOnlyKeyAndColon:
        |Resources:
        |  MemberWithOnlyKey
        |  LaunchConfig:
        |    Type: AWS
      """.stripMargin
    val compilation = language.compileString(program)
    assert(compilation.diagnostics.size == 2)
  }

  private def replaceDefaultWithDefaultString(compilation: Compilation): Unit = {
    compilation.program.asInstanceOf[PathRoot].visitShape(ExpressionDelta.DefaultShape,
      p => p.asInstanceOf[ChildPath].replaceWith(StringLiteralDelta.Shape.neww("default")))
  }

  test("tagged block key") {
    val input = """      UserData: !Base64
                  |        Fn::Join:
                  |          - ''
                  |          - ['#!/bin/bash -xe
                  |
                  |            ', 'yum update -y aws-cfn-bootstrap
                  |
                  |            ', '/opt/aws/bin/cfn-init -v ', '         --stack ', !Ref 'AWS::StackName',
                  |             '         --resource LaunchConfig ', '         --region ', !Ref 'AWS::Region',
                  |             '
                  |
                  |            ', '/opt/aws/bin/cfn-signal -e $? ', '         --stack ', !Ref 'AWS::StackName',
                  |             '         --resource WebServerGroup ', '         --region ', !Ref 'AWS::Region',
                  |             '
                  |
                  |            ']
                  |""".stripMargin

    val compilation = language.compileString(input)
    assert(compilation.diagnostics.isEmpty)
  }

  test("plain scalar") {
    val contents =
      """Blaa
        |Comment
      """.stripMargin

    val language = TestLanguageBuilder.buildWithParser(Seq(new SelectGrammar(ExpressionDelta.FirstPrecedenceGrammar),
      PlainScalarDelta, JsonObjectLiteralDelta, ExpressionDelta))
    val compilation = language.compileString(contents)
    assert(compilation.diagnostics.nonEmpty)
  }

  val deltas = Seq(new SelectGrammar(YamlCoreDelta.BlockValue),
    YamlObjectDelta, YamlArrayDelta, YamlCoreDelta, ArrayLiteralDelta, PlainScalarDelta, JsonObjectLiteralDelta, ExpressionDelta)
  val blockLanguage = TestLanguageBuilder.buildWithParser(deltas,
    stopFunction = UntilBestAndXStepsStopFunction(), indentationSensitive = true)

  test("plain scalar 2") {
    val contents =
      """Metadata:
        |  Blaa
        |  Comment""".stripMargin

    val compilation = blockLanguage.compileString(contents)
    assert(compilation.diagnostics.nonEmpty)
  }

  test("missing :value error correction") {
    val contents =
      """A: B
        |C
        |D: E""".stripMargin

    val compilation = blockLanguage.compileString(contents)
    assert(compilation.diagnostics.size == 1)
    assert(compilation.diagnostics.head.diagnostic.message.contains(":<value>"))
  }

  test("composite") {
    val contents =
      """Metadata:
        |  Comment: Install a simple application
        |  Blaa
        |  AWS: Bar""".stripMargin

    val compilation = blockLanguage.compileString(contents)
    assert(compilation.diagnostics.size == 1)
    assert(compilation.diagnostics.head.diagnostic.message.contains(":<value>"))
  }

  test("big yaml file") {
    val contents = SourceUtils.getResourceFileContents("yaml/AutoScalingMultiAZWithNotifications.yaml")

    val compilation = language.compileString(contents)
    assert(compilation.diagnostics.isEmpty)
  }

  test("large recursion yaml") {
    val contents = SourceUtils.getResourceFileContents("yaml/LargeRecursionyaml")

    val compilation = language.compileString(contents)
    assert(compilation.diagnostics.isEmpty)
  }
}
