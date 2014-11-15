package transformations.bytecode.constants

trait NameAndType {

  object NameAndTypeKey

  object NameAndTypeName

  object NameAndTypeType

  def nameAndType(nameIndex: Int, typeIndex: Int): MetaObject = new MetaObject(NameAndTypeKey) {
    data.put(NameAndTypeName, nameIndex)
    data.put(NameAndTypeType, typeIndex)
  }

  val nameAndTypeGrammar = "name and type: " ~~> (number <~ ":") ~ number ^^ parseMap(NameAndTypeKey, NameAndTypeName, NameAndTypeType)

  def getNameAndTypeName(nameAndType: MetaObject) = nameAndType(NameAndTypeName).asInstanceOf[Int]

  def getNameAndTypeType(nameAndType: MetaObject) = nameAndType(NameAndTypeType).asInstanceOf[Int]

}
