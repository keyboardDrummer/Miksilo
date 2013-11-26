package ssm

class Program(instruction: Seq[Instruction]) {

}

abstract class Instruction
{
  def command: String
  def arguments: Seq[String]
}

case class Label(target: String) extends Instruction{
  def command: String = "label"
  def arguments: Seq[String] = Seq(target)
}
