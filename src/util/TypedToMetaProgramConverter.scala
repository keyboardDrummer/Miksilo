package util

import transformation.MetaObject

class TypedToMetaProgramConverter {
  def convert(program: AnyRef): MetaObject = {
    convertAny(program).asInstanceOf[MetaObject]
  }

  def convertAny(value: Any): Any = {
    val clazz = value.getClass
    if (clazz.isPrimitive) {
      value
    }
    else value match
    {
      case _: Integer => value
      case _: String => value
      case seq: Seq[_] => seq.map(convertAny)
      case _ =>
        val clazz = value.getClass
        val result = new MetaObject(clazz.getSimpleName)
        for (field <- clazz.getDeclaredFields) {
          try {
            val getter = clazz.getDeclaredMethod(field.getName)
            result(field.getName) = convertAny(getter.invoke(value))
          } catch {
            case _: NoSuchMethodException =>
            case _: IllegalAccessException =>
          }
        }
        result
    }
  }
}
