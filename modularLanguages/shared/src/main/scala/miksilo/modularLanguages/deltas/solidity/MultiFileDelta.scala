package miksilo.modularLanguages.deltas.solidity

import miksilo.modularLanguages.core.deltas.path.{NodePath, PathRoot}
import miksilo.modularLanguages.core.deltas.{DeltaWithPhase, ShapeProperty}
import miksilo.languageServer.core.language.{Compilation, CompilationCache}
import miksilo.modularLanguages.core.node._
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.ConstraintSkeleton
import miksilo.modularLanguages.deltas.classes.HasConstraintsDelta

import scala.collection.mutable

object MultiFileDelta extends DeltaWithPhase with HasConstraintsDelta {

  object Shape extends NodeShape
  object Files extends NodeField

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val phases = compilation.language.compilerPhases.takeWhile(p => p.key != this)

    def compileFile(fileReference: String): Node = {
      val fileCompilation = new Compilation(new CompilationCache(compilation.language, compilation.fileSystem), Some(fileReference))
      for(phase <- phases) {
        phase.action(fileCompilation)
      }
      fileCompilation.program.asInstanceOf[PathRoot].current // TODO correctly handle the diagnostics from this other file. Move them to the other compilation?
    }

    var fileQueue = List[Node](program)
    val visitedFiles = new mutable.HashSet[String]()
    var files = List.empty[Node]
    visitedFiles.add(compilation.rootFile.get)
    while(fileQueue.nonEmpty) {
      val file = fileQueue.head
      files ::= file
      fileQueue = fileQueue.tail

      val references = getFileReferencesFromFile(compilation, file)
      for(reference  <- references) {
        if (visitedFiles.add(reference)) {
          fileQueue ::= compileFile(reference)
        }
      }
    }

    compilation.program = PathRoot(Shape.create(Files -> files))
  }

  trait HasFileReferences {
    def getReferences(node: Node): Seq[String]
  }
  val getFile = new ShapeProperty[HasFileReferences]

  def getFileReferencesFromFile(compilation: Compilation, file: Node): Seq[String] = {
    val fileMembers = file(FileWithMembersDelta.Members).asInstanceOf[Seq[Node]]
    fileMembers.flatMap(member =>
      getFile.get(compilation, member.shape).map(f => f.getReferences(member)).getOrElse(Seq.empty))
  }

  override def description = "Enables compiling multiple files"

  override def dependencies = Set.empty

  override def shape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    path(Files).asInstanceOf[Seq[NodePath]].foreach(file =>
      ConstraintSkeleton.constraints(compilation, builder, file, parentScope))
  }
}
