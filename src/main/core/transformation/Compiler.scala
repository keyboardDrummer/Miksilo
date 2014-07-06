package core.transformation

trait Compiler {
  def compile(program: MetaObject) : MetaObject
}
