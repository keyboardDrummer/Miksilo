package miksilo.editorParser.parsers

import miksilo.editorParser.languages.yaml.YamlParser
import miksilo.editorParser.parsers.editorParsers.UntilBestAndXStepsStopFunction
import org.scalatest.funsuite.AnyFunSuite

class ErrorCorrectionUsingYamlTest extends AnyFunSuite {

  test("Broken in the middle 2") {
    val program =
      """Parameters:
        |  KeyName: The EC2 Key Pair to allow SSH access to the instances
        |  MemberWithOnlyKeyAndColon:
        |Resources:
        |  MemberWithOnlyKey
        |  LaunchConfig:
        |    Type: AWS::AutoScaling::LaunchConfiguration
        |""".stripMargin
    parseYaml(program, null, 1, 100)
  }

  test("Broken in the middle") {
    val program =
      """Parameters:
        |  KeyName: The EC2 Key Pair to allow SSH access to the instances
        |  MemberWithOnlyKeyAndColon:
        |Resources:
        |  MemberWithOnlyKey
        |  LaunchConfig:
        |    Type: AWS::AutoScaling::LaunchConfiguration
        |    Properties:
        |      KeyName: !Ref 'KeyName'
      """.stripMargin
    parseYaml(program, null, 1, 10)
  }

  val yamlParser = YamlParser.parser

  // Add test for left recursion and errors
  // Add test with multiple errors in one branch "b" => "a" "b" "c"
  // Add test with three way branch with 0,1,2 errors, and 0,2,1 errors.
  private def parseYaml(input: String, expectation: Any, errorCount: Int, steps: Int = 0) = {
    val result = yamlParser.parse(input, UntilBestAndXStepsStopFunction(steps))
    val primitiveResult = YamlTestUtils.valueToPrimitive(result.resultOption.get)
    assertResult(expectation)(primitiveResult)
    assertResult(errorCount)(result.errors.size)
  }
}
