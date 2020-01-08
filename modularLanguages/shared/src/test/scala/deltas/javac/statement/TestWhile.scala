package deltas.javac.statement

import util.JavaLanguageTest

class TestWhile extends JavaLanguageTest {

  test("basic") {
    compareWithJavacAfterRunning("Whilee")
  }
}
