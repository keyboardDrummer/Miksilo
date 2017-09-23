package core.particles

trait Contract {
  def dependencies: Set[Contract] = Set.empty

  def name: String = {
    try
    {
      val simpleName = getClass.getSimpleName
      var result = simpleName.dropRight(1)
      if (result.last == 'C')
        result = result.dropRight(1)
      if (result.endsWith("Delta"))
        result = result.dropRight(5)
      splitCamelCase(result).toLowerCase
    }
    catch
    {
      case e: java.lang.InternalError => "internalError"
    }
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