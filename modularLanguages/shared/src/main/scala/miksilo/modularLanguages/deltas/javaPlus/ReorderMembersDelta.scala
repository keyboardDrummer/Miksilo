package miksilo.modularLanguages.deltas.javaPlus

import miksilo.modularLanguages.core.deltas.{Contract, Delta, DeltaWithPhase, LanguageFromDeltas}
import miksilo.modularLanguages.core.node.Node
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.PrettyPrint
import miksilo.modularLanguages.deltas.classes.ClassDelta.JavaClass
import miksilo.modularLanguages.deltas.javac.classes.skeleton.JavaClassDelta
import miksilo.modularLanguages.deltas.javac.methods.AccessibilityFieldsDelta.HasAccessibility
import miksilo.modularLanguages.deltas.javac.methods.{AccessibilityFieldsDelta, MethodDelta}
import miksilo.lspprotocol.lsp.LanguageServer

// TODO this thing is broken. Add tests+fix or remove.
object ReorderMembersDelta extends Delta {

  override def description: String = "Moves static before instance fields, and fields before members"

  override def dependencies: Set[Contract] = Set[Contract](JavaClassDelta, AccessibilityFieldsDelta, MethodDelta)

  override def inject(language: Language): Unit = {
    super.inject(language)
    //language.extraCompileOptions ::= ReorderOption
  }

  trait CustomCommand {
    def name: String

    def initialize(deltas: Seq[Delta]): Unit
    def perform(server: LanguageServer): Unit
  }

  object ReorderOption extends CustomCommand {

    val name = "ReorderMembers"

    val prettyPrint = PrettyPrint(recover = true)
    var language: Language = _
    override def initialize(deltas: Seq[Delta]): Unit = {
      language = LanguageFromDeltas(Seq(ActuallyReorderMembers, prettyPrint) ++ deltas)
    }

    override def perform(server: LanguageServer): Unit = {
//      val compilation = language.parseAndTransform(input)
//      val outputGrammar = prettyPrint.getOutputGrammar(compilation.language)
//      TextWithGrammar(compilation.output, outputGrammar)
    }
  }

  object ActuallyReorderMembers extends DeltaWithPhase {
    override def transformProgram(program: Node, state: Compilation): Unit = {
      val javaClass: JavaClass[Node] = program

      val methods = javaClass.members.filter(member => member.shape == MethodDelta.Shape)
      val fields = javaClass.members.filter(member => member.shape != MethodDelta.Shape)

      val orderedFields = fields.sortBy(f => !new HasAccessibility[Node] { def node = f }.isStatic)

      javaClass.members = orderedFields ++ methods
    }

    override def description: String = "Used by ReorderMembersDelta"

    override def dependencies: Set[Contract] = Set(JavaClassDelta)
  }
}
