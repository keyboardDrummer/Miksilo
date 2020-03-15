package miksilo.modularLanguages.deltas.javac.classes.skeleton

case class QualifiedClassName(parts: Seq[String]) {
  override def toString = parts.mkString(".")
}
