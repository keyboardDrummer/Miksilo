package ssm

case class SSMProgram(instructions: Seq[Instruction]) {
}

abstract class Instruction
{
  def command: String
  def execute(machine: SSMMachine)
}

case class Label(name: String) extends Instruction {
  def command = "label"

  def execute(machine: SSMMachine) {}
}

case class JumpOnFalse(target: String) extends Instruction {
  def command = "brf"

  def execute(machine: SSMMachine) {
    if (!machine.popBoolean())
      machine.goTo(target)
  }
}

case class JumpOnTrue(target: String) extends Instruction {
  def command = "brt"

  def execute(machine: SSMMachine) {
    if (machine.popBoolean())
      machine.goTo(target)
  }
}

case class JumpAlways(target: String) extends Instruction {
  def command = "bra"

  def execute(machine: SSMMachine) {
    machine.goTo(target)
  }
}

case class LoadRegister(index: Int) extends Instruction {
  def command = "ldr"

  def execute(machine: SSMMachine) {
    machine.push(machine.registers(index))
  }
}

case class StoreRegister(index: Int) extends Instruction {
  def command = "str"

  def execute(machine: SSMMachine) {
    machine.registers.put(index, machine.pop())
  }
}

case class LoadConstant(value: Int) extends Instruction {
  def command = "ldc"

  def execute(machine: SSMMachine): Unit = machine.push(value)
}

case class NotEquals extends Instruction {
  def command = "ne"

  def execute(machine: SSMMachine): Unit = {
    val first = machine.pop()
    val second = machine.pop()
    machine.push(first == second)
  }
}

case class Addition extends Instruction {
  def command = "add"

  def execute(machine: SSMMachine): Unit = {
    machine.push(machine.pop() + machine.pop())
  }
}