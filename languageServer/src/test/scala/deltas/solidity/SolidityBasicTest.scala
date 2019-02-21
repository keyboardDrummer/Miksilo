package deltas.solidity

import core.language.node.FileRange
import langserver.types.Position
import languageServer.{HumanPosition, LanguageServerTest, MiksiloLanguageServer}
import util.TestLanguageBuilder

class SolidityBasicTest extends LanguageServerTest {
  val solidity = TestLanguageBuilder.build(SolidityLanguage.deltas)

  test("contract, fields, functions, return expression, assignment") {
    val program =
      """pragma solidity ^0.4.0;
        |
        |contract MyFirstContract {
        |    string private name;
        |    uint private age;
        |
        |    function setName(string newName) public {
        |        name = newName;
        |    }
        |
        |    function getName() public view returns (string) {
        |        return name;
        |    }
        |
        |    function setAge(uint newAge) public {
        |        age = newAge;
        |    }
        |
        |    function getAge() public view returns (uint) {
        |        return age;
        |    }
        |}""".stripMargin

    val server = new MiksiloLanguageServer(SolidityLanguage.language)
    assertResult(Seq.empty)(getDiagnostics(server, program))

    val nameDeclaration: Seq[FileRange] = gotoDefinition(server, program, new HumanPosition(8,10))
    assert(nameDeclaration.head.range.contains(Position(3, 21)))
  }
}
