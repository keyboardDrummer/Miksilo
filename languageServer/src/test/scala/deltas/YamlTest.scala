
package deltas

import core.deltas.path.{ChildPath, PathRoot}
import core.language.Compilation
import deltas.expression.DefaultExpressionDelta
import deltas.json.StringLiteralDelta
import deltas.yaml.YamlLanguage
import org.scalatest.FunSuite
import util.{SourceUtils, TestLanguageBuilder}

class YamlTest extends FunSuite {

  val language = TestLanguageBuilder.buildWithParser(YamlLanguage.deltas)

  test("single member object without a value") {
    val program = "Key:"
    val compilation = language.compile(program)
    replaceDefaultWithDefaultString(compilation)

    val reference = "Key:'default'"
    val referenceCompilation = language.compile(reference)
    assertResult(referenceCompilation.program)(compilation.program)
    assert(compilation.diagnostics.size == 1)
  }

  val twoMemberObject =
    """Missing: default
      |Key: Value
    """.stripMargin
  val twoMemberObjectCompilation = language.compile(twoMemberObject)

  test("two member object with no first value") {
    val program =
      """Missing:
        |Key: Value
      """.stripMargin
    val compilation = language.compile(program)
    replaceDefaultWithDefaultString(compilation)

    val reference =
      """Missing: default
        |Key: Value
      """.stripMargin
    val referenceCompilation = language.compile(reference)
    assertResult(referenceCompilation.program)(compilation.program)
    assert(compilation.diagnostics.size == 1)
  }

  test("two member object with no first value and colon") {
    val program =
      """Missing
        |Key: Value
      """.stripMargin
    val compilation = language.compile(program)
    replaceDefaultWithDefaultString(compilation)

    assertResult(twoMemberObjectCompilation.program)(compilation.program)
    assert(compilation.diagnostics.size == 2)
  }

  private def replaceDefaultWithDefaultString(compilation: Compilation): Unit = {
    PathRoot(compilation.program).visitShape(DefaultExpressionDelta.Shape,
      p => p.asInstanceOf[ChildPath].replaceWith(StringLiteralDelta.neww("default")))
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

    val compilation = language.compile(input)
    assert(compilation.diagnostics.isEmpty)
  }

  test("big yaml file") {
    val contents = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.yaml")

    val compilation = language.compile(contents)
    assert(compilation.diagnostics.isEmpty)
  }
}
