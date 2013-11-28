package cLanguage

import util.StackedMap

class CMachine {
  val env = new StackedMap[String, Any]()
  env.push()
  def run(program: CProgram) {
    for(function <- program.functions)
      env.put(function.name, function)
    callFunction("main")
  }

  def callFunction(function: Function)
  {

  }
}
