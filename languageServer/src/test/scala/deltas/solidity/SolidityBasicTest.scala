package deltas.solidity

import core.language.node.FileRange
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

    val nameDeclarations: Seq[FileRange] = gotoDefinition(server, program, new HumanPosition(8,10))
    assert(nameDeclarations.head.range.contains(HumanPosition(4, 21)))

    val newAgeDeclarations: Seq[FileRange] = gotoDefinition(server, program, new HumanPosition(16,16))
    assert(newAgeDeclarations.head.range.contains(HumanPosition(15, 28)))
  }
}
