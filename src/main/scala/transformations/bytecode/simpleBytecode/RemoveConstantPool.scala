package transformations.bytecode.simpleBytecode

import core.bigrammar.GrammarReference
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import core.particles.path.PathRoot
import core.particles.{CompilationState, DeltaWithGrammar, DeltaWithPhase}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton.{ConstantPoolGrammar, ConstantPoolItemContentGrammar}
import transformations.bytecode.coreInstructions.ConstantPoolIndexGrammar
import transformations.javac.classes.ConstantPool

object RemoveConstantPool extends DeltaWithPhase with DeltaWithGrammar {
  override def transform(program: Node, state: CompilationState): Unit = {
    val pool = new ConstantPool()
    program(ByteCodeSkeleton.ClassConstantPool) = pool
    val constantReferences = ByteCodeSkeleton.getState(state).constantReferences

    PathRoot(program).visit(afterChildren = node => constantReferences.get(node.clazz).foreach(reference => {
      for (entry <- reference) {
        val fieldValue = node.current(entry._1)
        val index = pool.store(fieldValue)
        node.current.data.put(entry._1, index)
      }
    }))
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val constantEntry = grammars.find(ConstantPoolItemContentGrammar)
    for(path <- grammars.findPathsToKey(ConstantPoolIndexGrammar))
      path.set(constantEntry)

    val constantPoolGrammar = grammars.findPathsToKey(ConstantPoolGrammar).head
    constantPoolGrammar.previous.asInstanceOf[GrammarReference].removeMeFromSequence()
  }

  override def description: String = "Removes the constant pool in favor of inline constant entries"
}