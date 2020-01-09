package deltas.javac

import core.deltas.Delta
import core.deltas.path.{NodePath, PathRoot}
import core.language.node.Node
import core.language.{Compilation, Language, Phase}
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.ByteCodeSkeleton.ClassFile
import deltas.bytecode.constants.ClassInfoConstant
import deltas.javac.classes.{ClassCompiler, FieldToByteCode}
import deltas.javac.classes.skeleton.{FullyQualifyTypeReferences, JavaClassDelta}
import deltas.javac.classes.skeleton.JavaClassDelta.{JavaClass, Members}
import deltas.javac.methods.{MethodDelta, MethodToByteCode}

object JavaClassToByteCodeDelta extends Delta {

  override def inject(language: Language): Unit = {
    language.insertPhaseAfter(
      Phase(this, description, compilation => transformProgram(compilation.program.asInstanceOf[PathRoot].current, compilation)),
      FullyQualifyTypeReferences)
    super.inject(language)
  }

  def transformProgram(program: Node, compilation: Compilation): Unit = {
    transformClass(program)

    def transformClass(program: Node): Unit = {
      val javaClass: JavaClass[NodePath] = PathRoot(program)
      JavaStandardLibraryDelta.loadIntoClassPath(compilation)
      javaClass.node.shape = ByteCodeSkeleton.Shape
      val classFile = new ClassFile(javaClass.node)
      val classCompiler: ClassCompiler = ClassCompiler(javaClass.node, compilation)
      JavaClassDelta.state(compilation).classCompiler = classCompiler
      classCompiler.bind()

      val classInfo = classCompiler.currentClassInfo
      classFile.attributes = Seq()

      val classRef = classCompiler.getClassRef(classInfo)
      program(ByteCodeSkeleton.ClassNameIndexKey) = classRef
      val parentName = javaClass.parent.get
      val parentRef = ClassInfoConstant.classRef(classCompiler.fullyQualify(parentName))
      program(ByteCodeSkeleton.ClassParentIndex) = parentRef
      program(ByteCodeSkeleton.ClassInterfaces) = Seq()

      program(ByteCodeSkeleton.ClassFields) = JavaClassDelta.getFields[NodePath](javaClass).map(field =>
        FieldToByteCode.compile(compilation, field))

      program(ByteCodeSkeleton.Methods) = MethodDelta.getMethods[NodePath](javaClass).map(method =>
        MethodToByteCode.compile(compilation, method))

      javaClass.node.data.remove(Members)
    }
  }

  override def description = "Converts a Java class to JVM bytecode"

  override def dependencies = FieldToByteCode.dependencies ++ MethodToByteCode.dependencies
}
