package transformations.javac.classes

case class QualifiedClassName(parts: Seq[String]) {
  override def toString = parts.mkString("")
}
