package miksilo.modularLanguages.deltas.javac.classes.skeleton

import miksilo.languageServer.core.language.{Compilation, Language, Phase}
import miksilo.modularLanguages.core.SolveConstraintsDelta
import miksilo.modularLanguages.core.deltas.path.{FieldPath, PathRoot}
import miksilo.modularLanguages.core.deltas.{Contract, Delta}
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.bytecode.types.{QualifiedObjectTypeDelta, UnqualifiedObjectTypeDelta}
import miksilo.modularLanguages.deltas.classes.ClassDelta.JavaClass

object FullyQualifyTypeReferences extends Delta {
  override def description: String = "Replaces unqualified type references with qualified ones."

  override def inject(language: Language): Unit = {
    val phase = Phase(this, description, compilation => transformProgram(compilation.program.asInstanceOf[PathRoot].current, compilation))
    language.insertPhaseAfter(phase, SolveConstraintsDelta)
    super.inject(language)
  }

  def transformProgram(program: Node, compilation: Compilation): Unit = {
    PathRoot(program).visitShape(UnqualifiedObjectTypeDelta.Shape, _type => {
      val declaration = compilation.proofs.gotoDefinition(_type).get.origin.get.asInstanceOf[FieldPath].parent.current
      val clazz: JavaClass[Node] = declaration
      val parts = clazz._package ++ Seq(clazz.name)
      _type.replaceData(QualifiedObjectTypeDelta.neww(QualifiedClassName(parts)))
    })
  }

  override def dependencies: Set[Contract] = Set(QualifiedObjectTypeDelta, UnqualifiedObjectTypeDelta)
}
