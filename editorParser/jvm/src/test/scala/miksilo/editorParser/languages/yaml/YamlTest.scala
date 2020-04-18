
package miksilo.editorParser.languages.yaml

import miksilo.editorParser.SourceUtils
import org.scalatest.funsuite.AnyFunSuite

class YamlTest extends AnyFunSuite {
  val parser = YamlParser.parser

  test("scalars nested in bracket array") {
    val program = """AllowedValues: [m3.medium]""".stripMargin
    val result = parser.parse(program)
    assert(result.successful)
  }

  test("plainStyleMultilineLineInFlowCollection") {
    val input = """                  [<img src=", !FindInMap [Region2Examples, !Ref 'AWS::Region',
                  |                                              Examples], /cloudformation_graphic.png" alt="AWS CloudFormation
                  |                                                           Logo"/>, '<h1>Congratulations, you have successfully launched
                  |                    the AWS CloudFormation sample.</h1>']""".stripMargin
    val result = parser.parse(input)
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
    val result = parser.parse(input)
    assert(result.successful)
  }

  test("tag string") {
    val program = "!Taggy 'hello'"

    parseAndCompare(program, "Taggy" -> "hello")
  }

  test("string") {
    val program = "'hello'"

    parseAndCompare(program, "hello")
  }

  def parseAndCompare(program: String, primitive: Any): Unit = {
    val result = parser.parse(program)
    val primitiveResult = valueToPrimitive(result.resultOption.get)
    assertResult(primitive)(primitiveResult)
  }

  def valueToPrimitive(value: YamlValue): Any = {
    value match {
      case NumberLiteral(_, value) => Integer.parseInt(value)
      case StringLiteral(_, value) => value
      case YamlArray(_, elements) => elements.toList.map(e => valueToPrimitive(e))
      case YamlObject(_, members) => members.toList.map(e => (valueToPrimitive(e._1), valueToPrimitive(e._2)))
      case TaggedNode(_, tag, value) => tag -> valueToPrimitive(value)
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

    parseAndCompare(program, Seq("minecraft" -> 2))
  }

  test("object with 2 members") {
    val program =
      """minecraft: 2
        |cancelled: 3""".stripMargin

    parseAndCompare(program, Seq("minecraft" -> 2, "cancelled" -> 3))
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

    val expectation = Seq(Seq("x" -> 3, "y" -> 4))
    parseAndCompare(program, expectation)
  }

  test("array object composite 2") {
    val program =
      """- x: 3
        |  y: 4
        |- 2""".stripMargin

    val expectation = Seq(Seq("x" -> 3, "y" -> 4), 2)
    parseAndCompare(program, expectation)
  }

  test("array object composite") {
    val program =
      """- 2
        |- x: 3
        |  y: 4""".stripMargin

    val expectation = Seq(2, Seq("x" -> 3, "y" -> 4))
    parseAndCompare(program, expectation)
  }

  test("complex composite 2") {
    val program =
      """- a: - 1
        |- b: - 2""".stripMargin

    val expectation = Seq(
      Seq(
        "a" -> Seq(1)),
      Seq(
        "b" -> Seq(2)))
    parseAndCompare(program, expectation)
  }

  test("complex composite 2.5") {
    val program =
      """- x: 3
        |  y: a: 4
        |  z: - 2
        |     - 4""".stripMargin

    val expectation =
      Seq(
        Seq("x" -> 3,
          "y" -> Seq("a" -> 4),
          "z" -> Seq(2, 4)))
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
        Seq("x" -> 3,
          "y" -> Seq("a" -> 4, "b" -> 5),
          "z" -> Seq(2, 4)),
        6,
        Seq(
          "q" ->
            Seq(7, 8),
          "r" -> 9))
    parseAndCompare(program, expectation)
  }

  test("test empty bracket array nested") {
    val program = """Resources:
                    |  WebServerGroup:
                    |    Properties:
                    |      AvailabilityZones: !GetAZs ''
                    |      LoadBalancerNames: [!Ref 'ElasticLoadBalancer']""".stripMargin
    val result = parser.parse(program)
    assert(result.successful)
  }

  test("big yaml file") {
    val contents = SourceUtils.getResourceFileContents("AutoScalingMultiAZWithNotifications.yaml")
    val result = parser.parse(contents)
    assert(result.successful, result.toString)
  }

  test("large recursion yaml") {
    val blockScalarParser = YamlParser.blockScalar
    val contents = SourceUtils.getResourceFileContents("LargeRecursionyaml")
    val result = blockScalarParser.getWholeInputParser().parse(contents)
    assert(result.errors.isEmpty)
  }
}
