package deltas.javac

import util.JavaLanguageTest

class TestUnqualifiedString extends JavaLanguageTest {

  test("basic") {
    compareWithJavacAfterRunning("UnqualifiedString")
  }
}
