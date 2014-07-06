package typed.languages.javaBytecode

abstract class Instruction {
  def execute(machine: JavaByteCodeMachine)
}

class IntegerConst(value: Integer) extends Instruction{
  def execute(machine: JavaByteCodeMachine): Unit = machine.currentFrame.operands.push(value)
}

class ArrayLoad extends Instruction {
  def execute(machine: JavaByteCodeMachine): Unit = {
    val index = machine.operands.pop().asInstanceOf[Int]
    val array = machine.operands.pop().asInstanceOf[Array[_]]
    machine.currentFrame.operands.push(array(index))
  }
}

class ArrayStore extends Instruction {
  def execute(machine: JavaByteCodeMachine): Unit = {
    val value = machine.stack.pop()
    val index = machine.stack.pop().asInstanceOf[Int]
    val array = machine.stack.pop().asInstanceOf[Array[Any]]
    array.update(index,value)
  }
}

class NullConst extends Instruction {
  def execute(machine: JavaByteCodeMachine): Unit = machine.stack.push(null)
}

class IntegerLoad(index: Integer) extends Instruction{
  def execute(machine: JavaByteCodeMachine): Unit = machine.currentFrame.operands.push(machine.currentFrame.variables(index))
}

class IntegerStore(index: Integer) extends Instruction {
  def execute(machine: JavaByteCodeMachine): Unit = {
    val value = machine.currentFrame.operands.pop()
    machine.currentFrame.variables(index) = value
  }
}