package org.orbeon.saxon.expr

import java.{util => ju}

import org.orbeon.saxon.expr.instruct.{Actor, GlobalVariable, NamedTemplate}
import org.orbeon.saxon.om.{Function, StandardNames}
import org.orbeon.saxon.style.StylesheetPackage
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.Visibility.Visibility
import org.orbeon.saxon.trans.VisibilityProvenance.VisibilityProvenance
import org.orbeon.saxon.trans.{Mode, Visibility, VisibilityProvenance}
import org.orbeon.saxon.tree.util.FastStringBuffer

import scala.beans.BeanProperty
//import scala.collection.compat._
import scala.jdk.CollectionConverters._

object Component {

  def makeComponent(actor: Actor,
                    visibility: Visibility,
                    provenance: VisibilityProvenance,
                    containingPackage: StylesheetPackage,
                    declaringPackage: StylesheetPackage): Component = {
    val c =
      if (actor.isInstanceOf[Mode])
        new M
      else
        new Component

    c.actor             = actor
    c.visibility        = visibility
    c.provenance        = provenance
    c.containingPackage = containingPackage
    c.declaringPackage  = declaringPackage
    c
  }

  class M extends Component {

    override def getActor: Mode =
      super.getActor.asInstanceOf[Mode]

    def setActor(m: Mode): Unit =
      this.actor = m
  }
}

class Component private () {

   var actor: Actor = _

  @BeanProperty
  var visibility: Visibility = _

  private var bindings: ju.List[ComponentBinding] = new ju.ArrayList

  @BeanProperty
  var containingPackage: StylesheetPackage = _

  @BeanProperty
  var declaringPackage: StylesheetPackage = _

  private var provenance: VisibilityProvenance = _

  @BeanProperty
  var baseComponent: Component = _

  def getComponentBindings: ju.List[ComponentBinding] = bindings

  def setComponentBindings(bindings: ju.List[ComponentBinding]): Unit =
    this.bindings = bindings

  def setVisibility(visibility: Visibility,
                    provenance: VisibilityProvenance): Unit = {
    this.visibility = visibility
    this.provenance = provenance
  }

  def getVisibilityProvenance: VisibilityProvenance = provenance

  def isHiddenAbstractComponent: Boolean =
    visibility == Visibility.HIDDEN &&
      baseComponent != null         &&
      baseComponent.getVisibility == Visibility.ABSTRACT

  def getActor: Actor = actor

  def export(out: ExpressionPresenter,
             componentIdMap: ju.Map[Component, Integer],
             packageIdMap: ju.Map[StylesheetPackage, Integer]): Unit = {
    out.startElement("co")
    val id = obtainComponentId(this, componentIdMap)
    out.emitAttribute("id", "" + id)
    if (provenance != VisibilityProvenance.DEFAULTED)
      out.emitAttribute("vis", getVisibility.toString)
    val refs = listComponentReferences(componentIdMap)
    out.emitAttribute("binds", refs)
    if (baseComponent != null && getActor == baseComponent.getActor) {
      val baseId = obtainComponentId(baseComponent, componentIdMap)
      out.emitAttribute("base", "" + baseId)
      out.emitAttribute("dpack", packageIdMap.get(declaringPackage).toString)
    } else {
      actor.export(out)
    }
    out.endElement()
  }

  def listComponentReferences(
                               componentIdMap: ju.Map[Component, Integer]): String = {
    val fsb = new FastStringBuffer(128)
    for (ref <- getComponentBindings.asScala) {
      val target = ref.getTarget
      val targetId = obtainComponentId(target, componentIdMap)
      if (fsb.length != 0)
        fsb.append(" ")
      fsb.append("" + targetId)
    }
    fsb.toString
  }

  private def obtainComponentId(
                                 component: Component,
                                 componentIdMap: ju.Map[Component, Integer]): Int = {
    var id = componentIdMap.get(component)
    if (id == null) {
      id = componentIdMap.size
      componentIdMap.put(component, id)
    }
    id
  }

  def getComponentKind: Int =
    actor match {
      case _: NamedTemplate =>
        StandardNames.XSL_TEMPLATE
      case _: GlobalVariable =>
        StandardNames.XSL_VARIABLE
      case _: Function =>
        StandardNames.XSL_FUNCTION
      //    } else if (actor.isInstanceOf[AttributeSet]) {
      //      StandardNames.XSL_ATTRIBUTE_SET
      case _: Mode =>
        StandardNames.XSL_MODE
      case _ =>
        -1
    }
}
