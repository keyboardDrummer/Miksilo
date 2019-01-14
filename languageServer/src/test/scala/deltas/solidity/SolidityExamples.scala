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

  test("interface, function call, comment, various expressions") {

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

  test("jeroen") {

    val program = """pragma solidity ^0.5.0;
                    |
                    |contract EndUser {
                    |
                    |    /* An end user A can do the following:
                    |        1) Carry their personal data
                    |        2) List trusted authorities that can update the data
                    |        3) Subscribe to other end users' updates
                    |        4) Give permission to other end users to read user A's updates
                    |    */
                    |
                    |    // A user's personal data.
                    |    // States whether the user has @syphilis.
                    |    // Data about the user is issued by @issuer, at @dateIssued.
                    |    struct PersonalData {
                    |        bool syphilis;
                    |        uint256 dateIssued;
                    |        address issuer;
                    |    }
                    |
                    |    // Initialize user data storage
                    |    PersonalData private pdata;
                    |
                    |    // Initialize trusted data issuer registry
                    |    address[] private trustedIssuers;
                    |
                    |    address owner;
                    |
                    |    constructor ( ) public {
                    |        owner = msg.sender;
                    |    }
                    |
                    |    // A modifier restricting action to trusted data issuers
                    |    modifier onlyTrusted {
                    |        bool isTrusted = false;
                    |
                    |        // Check if msg.sender is a trusted issuer
                    |        for ( uint i = 0; i < trustedIssuers.length; i++)
                    |            if ( msg.sender == trustedIssuers[i] ) {
                    |                isTrusted = true;
                    |                break;
                    |            }
                    |
                    |        require ( isTrusted ); _;
                    |
                    |    }
                    |
                    |    modifier onlyOwner {
                    |        require ( msg.sender == owner ); _;
                    |    }
                    |
                    |    // Issue a data update to the end user, if sender is trusted
                    |    function updateData ( bool syphilisYet ) public onlyTrusted {
                    |        pdata.syphilis = syphilisYet;
                    |        pdata.dateIssued = now;
                    |        pdata.issuer = msg.sender;
                    |    }
                    |
                    |    // Add a new data issuer to the registry of trusted issuers
                    |    function startTrusting ( address newIssuer ) public onlyOwner {
                    |        trustedIssuers.push( newIssuer );
                    |    }
                    |
                    |    //function stopTrusting ( address badIssuer ) public onlyOwner {
                    |        // TODO does pop work by reference?
                    |        // TODO what if issuer not in list?
                    |      //  trustedIssuers.pop ( badIssuer );
                    |    //}
                    |}""".stripMargin

    val compilation = solidity.compile(program)
    assertResult(Seq.empty)(compilation.diagnostics)
  }

  test("strings") {

    val program = """pragma solidity ^0.4.0;
                    |
                    |library Strings {
                    |
                    |    function concat(string _base, string _value) pure internal returns (string) {
                    |        bytes memory _baseBytes = bytes(base);
                    |        bytes memory _valueBytes = bytes(_value);
                    |
                    |        string memory _tmpValue = new string(_baseBytes.length + _valueBytes.length);
                    |        bytes memory _newValue = bytes(_tmpValue);
                    |
                    |        uint i;
                    |        uint j;
                    |
                    |        for(i=0;i<_baseBytes.length;i++) {
                    |            _newValue[j++] = _baseBytes[i];
                    |        }
                    |
                    |        for(i=0;i<_valueBytes.length;i++) {
                    |            _newValue[j++] = _valueBytes[i];
                    |        }
                    |
                    |        return string(_newValue);
                    |    }
                    |
                    |    function strpos(string _base, string _value) pure internal returns (int) {
                    |        bytes memory _baseBytes = bytes(_base);
                    |        bytes memory _valueBytes = bytes(_value);
                    |
                    |        assert(_valueBytes.length == 1);
                    |
                    |        for(uint i=0;i<_baseBytes.length;i++) {
                    |            if (_baseBytes[i] == _valueBytes[0]) {
                    |                return int(i);
                    |            }
                    |        }
                    |
                    |        return -1;
                    |    }
                    |}""".stripMargin

    val compilation = solidity.compile(program)
    assertResult(Seq.empty)(compilation.diagnostics)
  }

  test("assembly") {

    val program = """pragma solidity ^0.4.0;
                    |
                    |contract Assembly {
                    |    function nativeLoops() public returns (uint _r) {
                    |        for(uint i = 0; i < 10; i++) {
                    |            _r++;
                    |        }
                    |    }
                    |
                    |    function asmLoops() public returns (uint _r) {
                    |        assembly {
                    |            let i := 0
                    |            loop:
                    |            i := add(i, 1)
                    |            _r := add(_r, 1)
                    |            jumpi(loop, lt(i, 10))
                    |        }
                    |    }
                    |
                    |    function nativeConditional(uint _v) public returns (uint) {
                    |        if (5 == _v) {
                    |            return 55;
                    |        } else if (6 == _v) {
                    |            return 66;
                    |        }
                    |        return 11;
                    |    }
                    |
                    |    function asmConditional(uint _v) public returns (uint _r) {
                    |        assembly {
                    |            switch _v
                    |            case 5 {
                    |                _r := 55
                    |            }
                    |            case 6 {
                    |                _r := 66
                    |            }
                    |            default {
                    |                _r := 11
                    |            }
                    |        }
                    |    }
                    |
                    |    function asmReturns(uint _v) public returns (uint) {
                    |        assembly {
                    |            let _ptr := add(msize(), 1)
                    |            mstore(_ptr, _v)
                    |            return(_ptr, 0x20)
                    |        }
                    |    }
                    |}""".stripMargin

    val compilation = solidity.compile(program)
    assertResult(Seq.empty)(compilation.diagnostics)
  }

  test("alarmTrigger") {

    val program = """pragma solidity ^0.4.0;
                    |
                    |interface AlarmWakeUp {
                    |    function callback(bytes _data) public;
                    |}
                    |
                    |contract AlarmService {
                    |
                    |    struct TimeEvent {
                    |        address addr;
                    |        bytes data;
                    |    }
                    |
                    |    mapping(uint => TimeEvent[]) private _events;
                    |
                    |    function set(uint _time)
                    |        public
                    |        returns (bool) {
                    |        TimeEvent _timeEvent;
                    |        _timeEvent.addr = msg.sender;
                    |        _timeEvent.data = msg.data;
                    |        _events[_time].push(_timeEvent);
                    |    }
                    |
                    |    function call(uint _time)
                    |        public {
                    |        TimeEvent[] timeEvents = _events[_time];
                    |        for(uint i = 0; i < timeEvents.length; i++) {
                    |            AlarmWakeUp(timeEvents[i].addr).callback(timeEvents[i].data);
                    |        }
                    |    }
                    |}
                    |
                    |contract AlarmTrigger is AlarmWakeUp {
                    |
                    |    AlarmService private _alarmService;
                    |
                    |    function AlarmTrigger() {
                    |        _alarmService = new AlarmService();
                    |    }
                    |
                    |    function callback(bytes _data)
                    |        public {
                    |        // Do something
                    |    }
                    |
                    |    function setAlarm()
                    |        public {
                    |        _alarmService.set(block.timestamp+60);
                    |    }
                    |
                    |}""".stripMargin

    val compilation = solidity.compile(program)
    assertResult(Seq.empty)(compilation.diagnostics)
  }

  test("casino") {

    val program = """pragma solidity ^0.4.0;
                    |
                    |contract Casino {
                    |
                    |    uint private start;
                    |
                    |    uint private buyPeriod = 1000;
                    |    uint private verifyPeriod = 100;
                    |    uint private checkPeriod = 100;
                    |
                    |    mapping(address => uint) private _tickets;
                    |    mapping(address => uint) private _winnings;
                    |
                    |    address[] _entries;
                    |    address[] _verified;
                    |
                    |    uint private winnerSeed;
                    |    bool private hasWinner;
                    |    address private winner;
                    |
                    |    function Casino()
                    |        public {
                    |        start = block.timestamp;
                    |    }
                    |
                    |    /**
                    |     * This should NOT be part of the contract!!
                    |     */
                    |    function unsafeEntry(uint number, uint salt)
                    |        public
                    |        payable
                    |        returns (bool) {
                    |        return buyTicket(generateHash(number, salt));
                    |    }
                    |
                    |    function generateHash(uint number, uint salt)
                    |        public
                    |        pure
                    |        returns (uint) {
                    |        return uint(keccak256(number + salt));
                    |    }
                    |
                    |    function buyTicket(uint hash)
                    |        public
                    |        payable
                    |        returns (bool) {
                    |        // Within the timeframe
                    |        require(block.timestamp < start+buyPeriod);
                    |        // Correct amount
                    |        require(1 ether == msg.value);
                    |        // 1 entry per address
                    |        require(_tickets[msg.sender] == 0);
                    |        _tickets[msg.sender] = hash;
                    |        _entries.push(msg.sender);
                    |        return true;
                    |    }
                    |
                    |    function verifyTicket(uint number, uint salt)
                    |        public
                    |        returns (bool) {
                    |        // Within the timeframe
                    |        require(block.timestamp >= start+buyPeriod);
                    |        require(block.timestamp < start+buyPeriod+verifyPeriod);
                    |        // Has a valid entry
                    |        require(_tickets[msg.sender] > 0);
                    |        // Validate hash
                    |        require(salt > number);
                    |        require(generateHash(number, salt) == _tickets[msg.sender]);
                    |        winnerSeed = winnerSeed ^ salt ^ uint(msg.sender);
                    |        _verified.push(msg.sender);
                    |    }
                    |
                    |    function checkWinner()
                    |        public
                    |        returns (bool) {
                    |        // Within the timeframe
                    |        require(block.timestamp >= start+buyPeriod+verifyPeriod);
                    |        require(block.timestamp < start+buyPeriod+verifyPeriod+checkPeriod);
                    |        if (!hasWinner) {
                    |            winner = _verified[winnerSeed % _verified.length];
                    |            _winnings[winner] = _verified.length-10 ether;
                    |            hasWinner = true;
                    |        }
                    |        return msg.sender == winner;
                    |    }
                    |
                    |    function claim()
                    |        public {
                    |        // Has winnings to claim
                    |        require(_winnings[msg.sender] > 0);
                    |        uint claimAmount = _winnings[msg.sender];
                    |        _winnings[msg.sender] = 0;
                    |        msg.sender.transfer(claimAmount);
                    |    }
                    |}""".stripMargin

    val compilation = solidity.compile(program)
    assertResult(Seq.empty)(compilation.diagnostics)
  }
}
