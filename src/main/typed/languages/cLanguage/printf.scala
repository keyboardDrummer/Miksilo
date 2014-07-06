package typed.languages.cLanguage

class PrintF(format: Expression, arguments: Expression*) extends Statement {
  val nullCharacter = '\0'
  def execute(machine: CMachine): StatementResult = {
    val value = format.evaluate(machine).asInstanceOf[Int]
    val chars = Stream.from(value).map(location => machine.memory(location)).map(v => v.asInstanceOf[Char])
      .takeWhile(c => c != nullCharacter)

    machine.standardOut.write(chars.toArray)
    Done
  }
}
