package cloudformation

import deltas.cloudformation.CloudFormationLanguage
import langserver.types._
import languageServer.{HumanPosition, LanguageServerTest, MiksiloLanguageServer}
import org.scalatest.FunSuite
import util.SourceUtils

class YamlCloudFormationTest extends FunSuite with LanguageServerTest {

  val yamlServer = new MiksiloLanguageServer(CloudFormationLanguage.yamlLanguage)

  test("No diagnostics") {
    val program = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.yaml")
    val result = getDiagnostic(yamlServer, program)
    assert(result.isEmpty)
  }

  test("Goto definition resource reference") {
    val program = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.yaml")
    val result: Seq[Location] = gotoDefinition(yamlServer, program, new HumanPosition(467, 32))
    assertResult(Seq(Location(itemUri, Range(new HumanPosition(443,3), new HumanPosition(336,25)))))(result)
  }
}
