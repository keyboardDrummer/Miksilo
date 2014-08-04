package core.transformation

trait Contract {
  def dependencies: Set[Contract] = Set.empty

  def name = {
    var result = getClass.getSimpleName.dropRight(1)
    if (result.last == 'C')
      result = result.dropRight(1)
    splitCamelCase(result).toLowerCase
  }

  def splitCamelCase(input: String): String = {
    input.replaceAll(
      String.format("%s|%s|%s",
        "(?<=[A-Z])(?=[A-Z][a-z])",
        "(?<=[^A-Z])(?=[A-Z])",
        "(?<=[A-Za-z])(?=[^A-Za-z])"
      ),
      " "
    )
  }
}