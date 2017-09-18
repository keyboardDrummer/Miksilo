package transformations.bytecode.simpleBytecode

import core.bigrammar.{As, GrammarReference}
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField}
import core.particles.path.PathRoot
import core.particles.{CompilationState, DeltaWithGrammar, DeltaWithPhase}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton.ConstantPoolGrammar
import transformations.bytecode.constants.ClassInfoConstant.{ClassRefKey, ClassRefName}
import transformations.bytecode.constants.FieldRefConstant.{FieldRefClassIndex, FieldRefNameAndTypeIndex}
import transformations.bytecode.constants.MethodRefConstant.{MethodRefClassName, MethodRefKey, MethodRefMethodName}
import transformations.bytecode.constants.NameAndTypeConstant.{NameAndTypeKey, NameAndTypeName, NameAndTypeType}
import transformations.bytecode.constants._
import transformations.bytecode.coreInstructions.ConstantPoolIndexGrammar
import transformations.bytecode.extraConstants.{QualifiedClassNameConstant, TypeConstant}
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

  override def transformGrammars(grammars: GrammarCatalogue, state: CompilationState): Unit = {
    val constantReferences = ByteCodeSkeleton.getState(state).constantReferences
    val allFields: Map[NodeField, NodeClass] = constantReferences.values.reduce((a, b) => a ++ b)

    for(path <- grammars.findPathsToKey(ConstantPoolIndexGrammar)) {
      val surroundingAs: As = path.ancestors.map(a => a.get).collect({case as:As => as}).head
      allFields.get(surroundingAs.key).foreach(constantClass => {
        path.set(grammars.find(constantClass))
      })
    }

    grammars.find(MethodRefConstant.key).inner = (grammars.find(ClassInfoConstant.key).as(MethodRefClassName) <~ "." ~
      grammars.find(NameAndTypeConstant.key).as(MethodRefMethodName)) asNode MethodRefKey
    grammars.find(ClassInfoConstant.key).inner = grammars.find(QualifiedClassNameConstant.key).as(ClassRefName) asNode ClassRefKey
    grammars.find(FieldRefConstant.key).inner = grammars.find(ClassInfoConstant.key).as(FieldRefClassIndex) ~ "/" ~
      grammars.find(NameAndTypeConstant.key).as(FieldRefNameAndTypeIndex) asNode FieldRefConstant.key
    grammars.find(NameAndTypeConstant.key).inner = grammars.find(Utf8Constant.key).as(NameAndTypeName) ~~
      grammars.find(TypeConstant.key).as(NameAndTypeType) asNode NameAndTypeKey

    val constantPoolGrammar = grammars.findPathsToKey(ConstantPoolGrammar).head
    constantPoolGrammar.previous.asInstanceOf[GrammarReference].removeMeFromSequence()
  }

  override def description: String = "Removes the constant pool in favor of inline constant entries"
}