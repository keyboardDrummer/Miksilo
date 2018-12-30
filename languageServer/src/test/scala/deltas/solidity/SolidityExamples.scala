package deltas.solidity

import org.scalatest.FunSuite
import util.TestLanguageBuilder

class SolidityExamples extends FunSuite {
  val solidity = TestLanguageBuilder.build(Solidity.deltas)

  test("My first solidity program") {
    val program = """pragma solidity ^0.4.0;
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

    val compilation = solidity.compile(program)
    assertResult(Seq.empty)(compilation.diagnostics)
  }
}
