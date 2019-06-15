package cloudformation

import core.bigrammar.TestLanguageGrammarUtils
import deltas.cloudformation.CloudFormationLanguage
import langserver.types._
import languageServer.lsp._
import languageServer.{HumanPosition, LanguageServerTest, MiksiloLanguageServer}
import org.scalatest.FunSuite
import util.SourceUtils

class CloudFormationTest extends FunSuite with LanguageServerTest {

  val jsonServer = new MiksiloLanguageServer(CloudFormationLanguage.jsonLanguage)

  test("No diagnostics") {
    val program = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.json")
    val result = getDiagnostic(jsonServer, program)
    assert(result.size == 1)
  }

  test("No diagnostics edited") {
    val program = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications_edited.json")
    val result = getDiagnostic(jsonServer, program)
    assert(result.size == 15) // TODO reduce the amount of errors.
  }

  test("Goto definition resource reference") {
    val program = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.json")
    val result: Seq[Location] = gotoDefinition(jsonServer, program, new HumanPosition(365, 37))
    assertResult(Seq(Location(itemUri, Range(new HumanPosition(336,6), new HumanPosition(336,28)))))(result)
  }

  test("Goto definition") {
    val program = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.json")
    val result: Seq[Location] = gotoDefinition(jsonServer, program, new HumanPosition(437, 36))
    assertResult(Seq(Location(itemUri, Range(new HumanPosition(42,6), new HumanPosition(42,17)))))(result)
  }

  test("Goto definition overloaded parameter") {
    val program = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.json")
    val result = gotoDefinition(jsonServer, program, new HumanPosition(445, 32))
    assertResult(Seq(Location(itemUri, Range(new HumanPosition(8,6), new HumanPosition(8,11)))))(result)
  }

  test("Goto definition overloaded parameter second") {
    val program = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.json")
    val result = gotoDefinition(jsonServer, program, new HumanPosition(425, 32))
    assertResult(Seq(Location(itemUri, Range(new HumanPosition(8,6), new HumanPosition(8,11)))))(result)
  }

  test("Code completion parameter") {
    val program = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.json")
    val result = complete(jsonServer, program, new HumanPosition(437, 38))
    val item = CompletionItem("SSHLocation", kind = Some(CompletionItemKind.Text), insertText = Some("SSHLocation"))
    assertResult(CompletionList(isIncomplete = false, Seq(item)))(result)
  }

  test("Code completion property") {
    val program = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.json")
    val result = complete(jsonServer, program, new HumanPosition(214, 14))
    val item = CompletionItem("Subscription", kind = Some(CompletionItemKind.Text), insertText = Some("Subscription"))
    assertResult(CompletionList(isIncomplete = false, Seq(item))) (result)
  }

  test("Code completion overloaded parameter") {
    val program = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.json")
    val result = complete(jsonServer, program, new HumanPosition(425, 32))
    val item = CompletionItem("VpcId", kind = Some(CompletionItemKind.Text), insertText = Some("VpcId"))
    assertResult(CompletionList(isIncomplete = false, Seq(item)))(result)
  }

  test("Parse example") {
    val utils = new TestLanguageGrammarUtils(CloudFormationLanguage.jsonDeltas)
    val source = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.json")
    utils.parse(source)
  }

  test("Code completion for property when most of the line is missing") {
    val program =
      """{
        |  "Resources" : {
        |    "NotificationTopic": {
        |      "Type": "AWS::SNS::Topic",
        |      "Properties": {
        |        "Subsc"
      """.stripMargin
    val server = new MiksiloLanguageServer(CloudFormationLanguage.jsonLanguage)
    val document = openDocument(server, program)
    val start = new HumanPosition(6, 14)
    val result = server.complete(DocumentPosition(document, start))

    val item = CompletionItem("Subscription", kind = Some(CompletionItemKind.Text), insertText = Some("Subscription"))
    assertResult(CompletionList(isIncomplete = false, Seq(item))) (result)
  }

  test("Missing in the middle") {
    val program =
      """{
        |  "Parameters" : {
        |    "KeyName": "The EC2 Key Pair to allow SSH access to the instances",
        |    "MemberWithOnlyKey":
        |  },
        |  "Resources" : {
        |    "LaunchConfig": {
        |      "Type": "AWS::AutoScaling::LaunchConfiguration",
        |      "Properties": {
        |        "KeyName": { "Ref": "KeyName" }
        |      }
        |    }
        |  }
        |}
        """.stripMargin
    val server = new MiksiloLanguageServer(CloudFormationLanguage.jsonLanguage)
    val document = openDocument(server, program)
    val start = new HumanPosition(10, 30)
    val result = server.gotoDefinition(DocumentPosition(document, start))

    assertResult(Seq(Location(itemUri, Range(new HumanPosition(3,6), new HumanPosition(3,13)))))(result)
  }

  test("file only has a string literal") {
    val program = "\"Foo\""
    val result = getDiagnostic(jsonServer, program)
    assert(result.isEmpty)
  }

  test("file has literal resources") {
    val program = """{"Resources" : "Bar"}"""
    val result = getDiagnostic(jsonServer, program)
    assert(result.isEmpty)
  }

  test("file has literal parameters") {
    val program = """{"Parameters" : "Bar"}"""
    val result = getDiagnostic(jsonServer, program)
    assert(result.isEmpty)
  }

  test("file only has a single member object") {
    val program = """{"Foo" : "Bar"}"""
    val result = getDiagnostic(jsonServer, program)
    assert(result.isEmpty)
  }
}
