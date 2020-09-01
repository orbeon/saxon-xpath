package net.sf.saxon.model

import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.om.Item
import net.sf.saxon.om.StructuredQName
import net.sf.saxon.tree.util.FastStringBuffer
import net.sf.saxon.value.ObjectValue
import JavaExternalObjectType._
import net.sf.saxon.utils.Configuration

object JavaExternalObjectType {

  def classNameToLocalName(className: String): String =
    className.replace('$', '-').replace("[", "_-")

  def localNameToClassName(className: String): String = {
    val fsb: FastStringBuffer = new FastStringBuffer(className.length)
    var atStart: Boolean = true
    for (k <- 0 until className.length) {
      var i = k
      val c: Char = className.charAt(i)
      if (atStart) {
        if (c == '_' && i + 1 < className.length && className.charAt(i + 1) == '-') {
          fsb.cat('[')
          i += 1
        } else {
          atStart = false
          fsb.cat(if (c == '-') '$' else c)
        }
      } else {
        fsb.cat(if (c == '-') '$' else c)
      }
    }
    fsb.toString
  }

  def classNameToQName(className: String): StructuredQName =
    new StructuredQName("jt",
      NamespaceConstant.JAVA_TYPE,
      classNameToLocalName(className))

}

class JavaExternalObjectType( var config: Configuration,
                              var javaClass: Class[_])
  extends ExternalObjectType {

  def getConfiguration: Configuration = config

  def getName(): String = javaClass.getName

  def getTargetNamespace(): String = NamespaceConstant.JAVA_TYPE

  def getTypeName(): StructuredQName = classNameToQName(javaClass.getName)

  override def getPrimitiveItemType: ItemType =
    config.getJavaExternalObjectType(classOf[AnyRef])

  def getRelationship(other: JavaExternalObjectType): Affinity.Affinity = {
    val j2: Class[_] = other.javaClass
    if (javaClass == j2) {
      Affinity.SAME_TYPE
    } else if (javaClass.isAssignableFrom(j2)) {
      Affinity.SUBSUMES
    } else if (j2.isAssignableFrom(javaClass)) {
      Affinity.SUBSUMED_BY
    } else if (javaClass.isInterface || j2.isInterface) {
      Affinity.OVERLAPS
    } else {
      Affinity.DISJOINT
    }
  }

  def getJavaClass: Class[_] = javaClass

  override def matches(item: Item, th: TypeHierarchy): Boolean = {
    if (item.isInstanceOf[ObjectValue[_]]) {
      val obj: AnyRef = item.asInstanceOf[ObjectValue[_]].getObject.asInstanceOf[AnyRef]
      javaClass.isAssignableFrom(obj.getClass)
    }
    false
  }

  override def toString: String =
    classNameToQName(javaClass.getName).getEQName

  def getDisplayName: String = "java-type:" + javaClass.getName

  override def hashCode(): Int = javaClass.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case obj: JavaExternalObjectType => javaClass == obj.javaClass
    case _ => false

  }

}
