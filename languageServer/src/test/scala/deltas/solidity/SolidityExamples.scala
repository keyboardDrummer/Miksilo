package deltas.solidity

import org.scalatest.FunSuite
import util.TestLanguageBuilder

class SolidityExamples extends FunSuite {
  val solidity = TestLanguageBuilder.build(SolidityLanguage.deltas)

  test("contract, fields, functions, return expression, assignment") {
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

  test("interface, funciton call, comment, various expressions") {

    val program = """pragma solidity ^0.4.0;
                    |
                    |interface Regulator {
                    |    function checkValue(uint amount) external returns (bool);
                    |    function loan() external returns (bool);
                    |}
                    |
                    |contract Bank is Regulator {
                    |    uint private value;
                    |
                    |    constructor(uint amount) public {
                    |        value = amount;
                    |    }
                    |
                    |    function deposit(uint amount) public {
                    |        value += amount;
                    |    }
                    |
                    |    function withdraw(uint amount) public {
                    |        if (checkValue(amount)) {
                    |            value -= amount;
                    |        }
                    |    }
                    |
                    |    function balance() public view returns (uint) {
                    |        return value;
                    |    }
                    |
                    |    function checkValue(uint amount) public returns (bool) {
                    |        // Classic mistake in the tutorial value should be above the amount
                    |        return value >= amount;
                    |    }
                    |
                    |    function loan() public returns (bool) {
                    |        return value > 0;
                    |    }
                    |}""".stripMargin

    val compilation = solidity.compile(program)
    assertResult(Seq.empty)(compilation.diagnostics)
  }

  test("assert/require/revert") {

    val program = """pragma solidity ^0.4.0;
                    |
                    |contract TestThrows {
                    |    function testAssert() public pure {
                    |        assert(1 == 2);
                    |    }
                    |
                    |    function testRequire() public pure {
                    |        require(2 == 1);
                    |    }
                    |
                    |    function testRevert() public pure {
                    |        revert();
                    |    }
                    |}""".stripMargin

    val compilation = solidity.compile(program)
    assertResult(Seq.empty)(compilation.diagnostics)
  }

  test("library import") {

    val program = """pragma solidity ^0.4.0;
                    |
                    |import "browser/library.sol";
                    |
                    |contract TestLibrary {
                    |    using IntExtended for uint;
                    |
                    |    function testIncrement(uint _base) public pure returns (uint) {
                    |        return IntExtended.increment(_base);
                    |    }
                    |
                    |    function testDecrement(uint _base) public pure returns (uint) {
                    |        return IntExtended.decrement(_base);
                    |    }
                    |
                    |    function testIncrementByValue(uint _base, uint _value) public pure returns (uint) {
                    |        return _base.incrementByValue(_value);
                    |    }
                    |
                    |    function testDecrementByValue(uint _base, uint _value) public pure returns (uint) {
                    |        return _base.decrementByValue(_value);
                    |    }
                    |}""".stripMargin

    val compilation = solidity.compile(program)
    assertResult(Seq.empty)(compilation.diagnostics)
  }

  test("transaction") {

    val program = """pragma solidity ^0.4.0;
                    |
                    |contract Transaction {
                    |
                    |    event SenderLogger(address);
                    |    event ValueLogger(uint);
                    |
                    |    address private owner;
                    |
                    |    modifier isOwner {
                    |        require(owner == msg.sender);
                    |        _;
                    |    }
                    |
                    |    modifier validValue {
                    |        assert(msg.value >= 1 ether);
                    |        _;
                    |    }
                    |
                    |    constructor() public {
                    |        owner = msg.sender;
                    |    }
                    |
                    |    function () public payable isOwner validValue {
                    |        emit SenderLogger(msg.sender);
                    |        emit ValueLogger(msg.value);
                    |    }
                    |}""".stripMargin

    val compilation = solidity.compile(program)
    assertResult(Seq.empty)(compilation.diagnostics)
  }

  test("datatypes") {

    val program = """pragma solidity ^0.4.0;
                    |
                    |contract DataTypes {
                    |
                    |    bool myBool = false;
                    |
                    |    int8 myInt = -128;
                    |    uint8 myUInt = 255;
                    |
                    |    string myString;
                    |    uint8[] myStringArr;
                    |
                    |    byte myValue;
                    |    bytes1 myBytes1;
                    |    bytes32 myBytes32;
                    |
                    |//    fixed256x8 myFixed = 1; // 255.0
                    |//    ufixed myFixed = 1;
                    |
                    |    enum Action {ADD, REMOVE, UPDATE}
                    |
                    |    Action myAction = Action.ADD;
                    |
                    |    address myAddress;
                    |
                    |    function assignAddress() public {
                    |        myAddress = msg.sender;
                    |        myAddress.balance;
                    |        myAddress.transfer(10);
                    |    }
                    |
                    |    uint[] myIntArr = [1,2,3];
                    |
                    |    function arrFunc() public {
                    |        myIntArr.push(1);
                    |        myIntArr.length;
                    |        myIntArr[0];
                    |    }
                    |
                    |    uint[10] myFixedArr;
                    |
                    |    struct Account {
                    |        uint balance;
                    |        uint dailyLimit;
                    |    }
                    |
                    |    Account myAccount;
                    |
                    |    function structFunc() public {
                    |        myAccount.balance = 100;
                    |    }
                    |
                    |    mapping (address => Account) _accounts;
                    |
                    |    function () public payable {
                    |        _accounts[msg.sender].balance += msg.value;
                    |    }
                    |
                    |    function getBalance() public view returns (uint) {
                    |        return _accounts[msg.sender].balance;
                    |    }
                    |}""".stripMargin

    val compilation = solidity.compile(program)
    assertResult(Seq.empty)(compilation.diagnostics)
  }
}
