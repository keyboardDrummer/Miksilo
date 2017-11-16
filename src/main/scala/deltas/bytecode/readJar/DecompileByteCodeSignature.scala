package deltas.bytecode.readJar

import core.deltas.node.Node
import core.deltas.{Compilation, DeltaWithPhase, Language}
import deltas.bytecode.ByteCodeSkeleton._
import deltas.bytecode.attributes.SignatureAttribute
import deltas.bytecode.constants.ClassInfoConstant
import deltas.bytecode.types.TypeSkeleton
import deltas.bytecode.{ByteCodeFieldInfo, ByteCodeMethodInfo, ByteCodeSkeleton}
import deltas.javac.classes.skeleton.{JavaClassSkeleton, QualifiedClassName}
import deltas.javac.classes.{ConstantPool, FieldDeclaration}
import deltas.javac.methods.AccessibilityFieldsDelta.Visibility
import deltas.javac.methods.{AccessibilityFieldsDelta, MethodDelta}
import deltas.javac.types.{MethodType, TypeAbstraction}

import scala.collection.mutable.ArrayBuffer

object DecompileByteCodeSignature extends DeltaWithPhase {
  override def transform(program: Node, state: Compilation): Unit = {
    val constantPool = program.constantPool
    val classReference = constantPool.getNode(program(ByteCodeSkeleton.ClassNameIndexKey).asInstanceOf[Int])
    val nameIndex = classReference(ClassInfoConstant.Name).asInstanceOf[Int]
    val qualifiedClassName = QualifiedClassName(constantPool.getUtf8(nameIndex).split("/").toSeq)

//    val parentClassReference = constantPool.getNode(program(ByteCodeSkeleton.ClassParentIndex).asInstanceOf[Int])
    //    val nameIndex = classReference(ClassRefConstant.ClassRefName).asInstanceOf[Int]
    //    val qualifiedClassName = new QualifiedClassName(constantPool.getUtf8(nameIndex).split("/").toSeq)

    val members = new ArrayBuffer[Node]()
    val javaClazz = JavaClassSkeleton.clazz(qualifiedClassName.parts.dropRight(1), qualifiedClassName.parts.last, members, List.empty[Node], None)
    
    val fieldInfos = program(ByteCodeSkeleton.ClassFields).asInstanceOf[Seq[Node]]

    members ++= getFields(state, constantPool, fieldInfos)
    members ++= getMethods(state, constantPool, program(ByteCodeSkeleton.Methods).asInstanceOf[Seq[Node]])

    program.replaceWith(javaClazz)
  }

  val accessFlagsToVisibility: Map[ByteCodeMethodInfo.MethodAccessFlag, Visibility] = AccessibilityFieldsDelta.visibilityAccessFlagLinks.
    flatMap(p => p._2.map(flag => (flag, p._1))).toMap

  def getMethods(state: Language, constantPool: ConstantPool, methodInfos: Seq[Node]): Seq[Node] = {
    methodInfos.map(methodInfo => {
      val nameIndex: Int = methodInfo(ByteCodeMethodInfo.MethodNameIndex).asInstanceOf[Int]
      val attributes = methodInfo(ByteCodeMethodInfo.MethodAttributes).asInstanceOf[Seq[Node]]
      val signatureAttribute = attributes.find(node => node.clazz == SignatureAttribute.key)
      val _type: Node = signatureAttribute match {
        case Some(signature) =>
          val signatureIndex = signature(SignatureAttribute.SignatureIndex).asInstanceOf[Int]
          val signatureTypeString = constantPool.getUtf8(signatureIndex)
          TypeSkeleton.getTypeFromByteCodeString(state, signatureTypeString)
        case None =>
          val methodDescriptorIndex = methodInfo(ByteCodeMethodInfo.MethodDescriptor).asInstanceOf[Int]
          val descriptor = constantPool.getUtf8(methodDescriptorIndex)
          val descriptorType: Node = TypeSkeleton.getTypeFromByteCodeString(state, descriptor)
          descriptorType
      }
      val name: String = constantPool.getUtf8(nameIndex)

      val (methodType, typeParameters) =
        if (_type.clazz == TypeAbstraction.TypeAbstractionKey)
          (TypeAbstraction.getBody(_type),TypeAbstraction.getParameters(_type))
        else
          (_type,Seq.empty)
      val returnType = methodType(MethodType.ReturnType).asInstanceOf[Node]
      val parameterTypes = methodType(MethodType.Parameters).asInstanceOf[Seq[Node]]
	    val parameters = parameterTypes.zipWithIndex.map(parameterTypeWithIndex =>
        MethodDelta.parameter("parameter" + parameterTypeWithIndex._2, parameterTypeWithIndex._1))

      val accessFlags: Set[ByteCodeMethodInfo.MethodAccessFlag] = Set.empty //TODO fix.
      val foundVisibilities: Set[Visibility] = accessFlags.flatMap(f => accessFlagsToVisibility.get(f))
      val visibility: Visibility = (foundVisibilities ++ Seq(AccessibilityFieldsDelta.DefaultVisibility)).head
      val static: Boolean = false //TODO fix.
      MethodDelta.method(name, returnType, parameters, Seq.empty, static, visibility, typeParameters)
    })
  }

  def getFields(state: Language, constantPool: ConstantPool, fieldInfos: Seq[Node]): Seq[Node] = {
    fieldInfos.map(fieldInfo => {
      val nameIndex: Int = fieldInfo(ByteCodeFieldInfo.NameIndex).asInstanceOf[Int]
      val attributes = fieldInfo(ByteCodeFieldInfo.FieldAttributes).asInstanceOf[Seq[Node]]
      val signatureAttribute = attributes.find(node => node.clazz == SignatureAttribute.key)
      val _type: Node = signatureAttribute match {
        case Some(signature) =>
          val signatureIndex = signature(SignatureAttribute.SignatureIndex).asInstanceOf[Int]
          val signatureTypeString = constantPool.getUtf8(signatureIndex)
          TypeSkeleton.getTypeFromByteCodeString(state, signatureTypeString)
        case None =>
          val fieldDescriptorIndex = fieldInfo(ByteCodeFieldInfo.DescriptorIndex).asInstanceOf[Int]
          val descriptor = constantPool.getUtf8(fieldDescriptorIndex)
          val descriptorType: Node = TypeSkeleton.getTypeFromByteCodeString(state, descriptor)
          descriptorType
      }
      val name: String = constantPool.getUtf8(nameIndex)
      FieldDeclaration.field(_type, name)      
    })
  }

  override def description: String = "Decompiles the field and method signatures in a classfile."
}
