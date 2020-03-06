
package core.parsers

import _root_.core.SourceUtils
import languages.yaml._
import org.scalatest.funsuite.AnyFunSuite

class YamlTest extends AnyFunSuite {
  val parser = YamlParser.parser

  test("plainStyleMultineLineInFlowCollection") {
    val input = """                  [<img src=", !FindInMap [Region2Examples, !Ref 'AWS::Region',
                  |                                              Examples], /cloudformation_graphic.png" alt="AWS CloudFormation
                  |                                                           Logo"/>, '<h1>Congratulations, you have successfully launched
                  |                    the AWS CloudFormation sample.</h1>']""".stripMargin
    val result = parser.resetAndParse(input)
    assert(result.successful)
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
    val result = parser.resetAndParse(input)
    assert(result.successful)
  }


  test("string") {
    val program = "'hello'"

    parseAndCompare(program, "hello")
  }

  def parseAndCompare(program: String, primitive: Any): Unit = {
    val result = parser.resetAndParse(program)
    assertResult(primitive)(valueToPrimitive(result.resultOption.get))
  }

  def valueToPrimitive(value: YamlValue): Any = {
    value match {
      case NumberLiteral(_, value) => value
      case StringLiteral(_, value) => value
      case YamlArray(_, elements) => elements.map(valueToPrimitive)
      case YamlObject(_, members) => members.map(e => (valueToPrimitive(e._1), valueToPrimitive(e._2)))
      case ValueHole(_) => null
    }
  }

  test("number") {
    val program = "3"

    parseAndCompare(program, 3)
  }

  test("object with single member") {
    val program =
      """minecraft: 2""".stripMargin

    parseAndCompare(program, Map("minecraft" -> 2))
  }

  test("object with 2 members") {
    val program =
      """minecraft: 2
        |cancelled: 3""".stripMargin

    parseAndCompare(program, Map("minecraft" -> 2, "cancelled" -> 3))
  }

  test("array") {
    val program =
      """- 2
        |- 3""".stripMargin

    parseAndCompare(program, Seq(2, 3))
  }

  test("object nested in singleton array") {
    val program =
      """- x: 3
        |  y: 4""".stripMargin

    val expectation = Seq(Map("x" -> 3, "y" -> 4))
    parseAndCompare(program, expectation)
  }

  test("array object composite 2") {
    val program =
      """- x: 3
        |  y: 4
        |- 2""".stripMargin

    val expectation = Seq(Map("x" -> 3, "y" -> 4), 2)
    parseAndCompare(program, expectation)
  }

  test("array object composite") {
    val program =
      """- 2
        |- x: 3
        |  y: 4""".stripMargin

    val expectation = Seq(2, Map("x" -> 3, "y" -> 4))
    parseAndCompare(program, expectation)
  }

  test("complex composite 2") {
    val program =
      """- a: - 1
        |- b: - 2""".stripMargin

    val expectation = Seq(
      Map(
        "a" -> Seq(1)),
      Map(
        "b" -> Seq(2)))
    parseAndCompare(program, expectation)
  }

  test("complex composite 3") {
    val program =
      """- 2
        |- x: 3
        |  y: a: 4
        |     b: 5
        |  z: - 2
        |     - 4
        |- 6
        |- q: - 7
        |     - 8
        |  r: 9""".stripMargin

    val expectation =
      Seq(
        2,
        Map("x" -> 3,
          "y" -> Map("a" -> 4, "b" -> 5),
          "z" -> Seq(2, 4)),
        6,
        Map(
          "q" ->
            Seq(7, 8),
          "r" -> 9))
    parseAndCompare(program, expectation)
  }

  test("big yaml file") {
    val contents = SourceUtils.getResourceFileContents("AutoScalingMultiAZWithNotifications.yaml")
    val result = parser.resetAndParse(contents)
    assert(result.successful, result.toString)
  }
}
