package deltas.solidity

import core.language.node.FileRange
import languageServer.{HumanPosition, LanguageServerTest, MiksiloLanguageServer}
import util.TestLanguageBuilder

class AlarmTriggerTest extends LanguageServerTest {
  val solidity = TestLanguageBuilder.build(SolidityLanguage.deltas)

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
                    |        //_events[_time].push(_timeEvent);
                    |    }
                    |
                    |    function call(uint _time)
                    |        public {
                    |        TimeEvent[] timeEvents = _events[_time];
                    |        for(uint i = 0; i < 3 /*timeEvents.length*/; i++) {
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

    val server = new MiksiloLanguageServer(SolidityLanguage.language)
    assertResult(Seq.empty)(getDiagnostics(server, program))

    val timeEventsDeclarations: Seq[FileRange] = gotoDefinition(server, program, new HumanPosition(29,28))
    assert(timeEventsDeclarations.head.range.contains(HumanPosition(27, 23)))

    val callbackDeclarations: Seq[FileRange] = gotoDefinition(server, program, new HumanPosition(29,50))
    assert(callbackDeclarations.head.range.contains(HumanPosition(4, 15)))

    val addrDeclarations: Seq[FileRange] = gotoDefinition(server, program, new HumanPosition(20,21))
    assert(addrDeclarations.head.range.contains(HumanPosition(10, 18)))
  }
}
