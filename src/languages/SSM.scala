package languages

import transformation.MetaObject

object SSM {
  def createInstruction(command: String, arguments: String*) = {
    new MetaObject("instruction") { data.put("command", command); data.put("arguments", arguments)}
  }
  val instruction = "instruction"
  val lines = "lines"

  def createLabel(target: String) = createInstruction("label",target)
  def jumpOnFalse(target: String) = createInstruction("brf",target)
  def jumpOnTrue(target: String) = createInstruction("brt",target)
  def jumpAlways(target: String) = createInstruction("bra", target)
}
