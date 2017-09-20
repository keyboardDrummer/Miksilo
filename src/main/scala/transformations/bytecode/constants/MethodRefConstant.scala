package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.coreInstructions.ConstantPoolIndexGrammar

object MethodRefConstant extends ConstantEntry {

  object MethodRefKey extends NodeClass

  object MethodRefClassName extends NodeField

  object MethodRefMethodName extends NodeField

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = {
    byteToBytes(10) ++
      shortToBytes(getMethodRefClassRefIndex(constant)) ++
      shortToBytes(getNameAndTypeIndex(constant))
  }

  override def key = MethodRefKey

  def methodRef(classNameIndex: Node, methodNameAndTypeIndex: Node) = new Node(MethodRefKey,
    MethodRefClassName -> classNameIndex,
    MethodRefMethodName -> methodNameAndTypeIndex)

  def methodRef(classNameIndex: Int, methodNameAndTypeIndex: Int) = new Node(MethodRefKey,
    MethodRefClassName -> classNameIndex,
    MethodRefMethodName -> methodNameAndTypeIndex)

  def getMethodRefClassRefIndex(methodRef: Node): Int = methodRef(MethodRefClassName).asInstanceOf[Int]

  def getNameAndTypeIndex(methodRef: Node): Int = methodRef(MethodRefMethodName).asInstanceOf[Int]

  def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar = "method reference:" ~~>
    (grammars.find(ConstantPoolIndexGrammar).as(MethodRefClassName) <~ "." ~
    grammars.find(ConstantPoolIndexGrammar).as(MethodRefMethodName)) asNode MethodRefKey

  override def description: String = "Defines the method reference constant, which refers to a method by class name, method name and signature."

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).constantReferences.put(key,
      Map(MethodRefClassName -> ClassInfoConstant.key, MethodRefMethodName -> NameAndTypeConstant.key))
  }
}




//En dan twee transformaties, 1 is RemoveConstantPool die de constantEntry grammars niet veranderd.
//En de tweede is een JasminConstantPoolSyntax
