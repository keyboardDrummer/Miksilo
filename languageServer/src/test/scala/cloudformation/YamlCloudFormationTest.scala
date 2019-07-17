package cloudformation

import deltas.cloudformation.CloudFormationLanguage
import languageServer._
import org.scalatest.FunSuite
import util.SourceUtils

class YamlCloudFormationTest extends FunSuite with LanguageServerTest {

  val yamlServer = new MiksiloLanguageServer(CloudFormationLanguage.yamlLanguage)

  test("No diagnostics") {
    val program = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.yaml")
    val result = getDiagnostic(yamlServer, program)
    assert(result.isEmpty)
  }

  test("Edited") {
    val program = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications_edited.yaml")
    val result = getDiagnostic(yamlServer, program)
    assertResult(2)(result.size)
  }

  test("Edited 2") {
    val program = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications_edited2.yaml")
    val result = getDiagnostic(yamlServer, program)
    assertResult(1)(result.size)
  }

  test("Goto definition resource reference") {
    val program = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.yaml")
    val result: Seq[FileRange] = gotoDefinition(yamlServer, program, new HumanPosition(467, 32))
    val expectation = Seq(FileRange(itemUri, SourceRange(new HumanPosition(443, 3), new HumanPosition(443, 25))))
    assertResult(expectation)(result)
  }

  test("Rename resource") {
    val program = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.yaml")
    val result: WorkspaceEdit = rename(yamlServer, program, new HumanPosition(467, 32), "boop")
    val expectation = WorkspaceEdit(Map(
      itemUri -> Seq(
        TextEdit(SourceRange(new HumanPosition(443, 3), new HumanPosition(443, 25)), "boop"),
        TextEdit(SourceRange(new HumanPosition(467, 28), new HumanPosition(467, 50)), "boop")
      )
    ))
    assertResult(expectation)(result)
  }

  test("Document symbols") {
    val program = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.yaml")
    val result: Set[SymbolInformation] = documentSymbols(yamlServer, program).toSet
    val expectation = Set(
      SymbolInformation("CPUAlarmLow",13,FileRange(itemUri,SourceRange(Position(471,2),Position(471,13))),None),
      SymbolInformation("WebServerGroup",13,FileRange(itemUri,SourceRange(Position(344,2),Position(344,16))),None),
      SymbolInformation("InstanceSecurityGroup",13,FileRange(itemUri,SourceRange(Position(501,2),Position(501,23))),None),
      SymbolInformation("InstanceType",13,FileRange(itemUri,SourceRange(Position(12,2),Position(12,14))),None),
      SymbolInformation("NotificationTopic",13,FileRange(itemUri,SourceRange(Position(338,2),Position(338,19))),None),
      SymbolInformation("OperatorEMail",13,FileRange(itemUri,SourceRange(Position(25,2),Position(25,15))),None),
      SymbolInformation("LaunchConfig",13,FileRange(itemUri,SourceRange(Position(366,2),Position(366,14))),None),
      SymbolInformation("KeyName",13,FileRange(itemUri,SourceRange(Position(30,2),Position(30,9))),None),
      SymbolInformation("WebServerScaleUpPolicy",13,FileRange(itemUri,SourceRange(Position(442,2),Position(442,24))),None),
      SymbolInformation("CPUAlarmHigh",13,FileRange(itemUri,SourceRange(Position(456,2),Position(456,14))),None),
      SymbolInformation("WebServerScaleDownPolicy",13,FileRange(itemUri,SourceRange(Position(449,2),Position(449,26))),None),
      SymbolInformation("SSHLocation",13,FileRange(itemUri,SourceRange(Position(34,2),Position(34,13))),None),
      SymbolInformation("ElasticLoadBalancer",13,FileRange(itemUri,SourceRange(Position(486,2),Position(486,21))),None))

    assertResult(expectation)(result)
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
    val result: Seq[FileRange] = gotoDefinition(yamlServer, program, new HumanPosition(9, 24))
    val expectation = Seq(FileRange(itemUri, SourceRange(new HumanPosition(2, 3), new HumanPosition(2, 10))))
    assertResult(expectation)(result)
  }
}
