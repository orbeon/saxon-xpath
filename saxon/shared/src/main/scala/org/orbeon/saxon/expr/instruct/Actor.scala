package org.orbeon.saxon.expr.instruct

import java.util.{Collections, Iterator, List}

import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.instruct.Actor._
import org.orbeon.saxon.expr.parser.RetainedStaticContext
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.om.{StandardNames, StructuredQName}
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.style.StylesheetPackage
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.{SymbolicName, Visibility, VisibilityProvenance}
import org.orbeon.saxon.trans.VisibilityProvenance.VisibilityProvenance

import scala.beans.BeanProperty
//import scala.collection.compat._
import scala.jdk.CollectionConverters._

object Actor {

  def allocateBindingSlotsRecursive(pack: StylesheetPackage,
                                    p: Actor,
                                    exp: Expression,
                                    bindings: List[ComponentBinding]): Unit = {
    exp match {
      case componentInvocation: ComponentInvocation =>
        p.processComponentReference(pack, componentInvocation, bindings)
      case _ =>
    }
    for (o <- exp.operands.asScala)
      allocateBindingSlotsRecursive(pack, p, o.getChildExpression, bindings)
  }
}

abstract class Actor extends ExpressionOwner with Location {

  var body: Expression = _
  @BeanProperty
  var systemId: String = _
  @BeanProperty
  var lineNumber: Int = _
  var stackFrameMap: SlotManager = _
  @BeanProperty
  var packageData: PackageData = _
  @BeanProperty
  var declaringComponent: Component = _
  @BeanProperty
  var declaredVisibility: Visibility.Visibility = _
  @BeanProperty
  var retainedStaticContext: RetainedStaticContext = _

  def getSymbolicName: SymbolicName
  def getComponentName: StructuredQName = getSymbolicName.getComponentName
  def getTracingTag: String = StandardNames.getLocalName(getSymbolicName.getComponentKind)

  def makeDeclaringComponent(visibility: Visibility.Visibility,
                             declaringPackage: StylesheetPackage): Component = {
    if (declaringComponent == null)
      declaringComponent = Component.makeComponent(
        this,
        visibility,
        VisibilityProvenance.DEFAULTED,
        declaringPackage,
        declaringPackage)
    declaringComponent
  }

  def obtainDeclaringComponent(/*declaration: StyleElement*/): Component = {
    // StyleElement not exist
    if (declaringComponent == null) {
      val declaringPackage: StylesheetPackage = null //declaration.getContainingPackage
      val defaultVisibility: Visibility.Visibility = /*if (declaration.isInstanceOf[XSLGlobalParam]) Visibility.PUBLIC else*/ Visibility.PRIVATE
      val declaredVisibility: Visibility.Visibility = null //declaration.getDeclaredVisibility
      val actualVisibility: Visibility.Visibility =
        if (declaredVisibility == null) defaultVisibility
        else declaredVisibility
      val provenance: VisibilityProvenance =
        if (declaredVisibility == null) VisibilityProvenance.DEFAULTED
        else VisibilityProvenance.EXPLICIT
      declaringComponent = Component.makeComponent(this,
        actualVisibility,
        provenance,
        declaringPackage,
        declaringPackage)
    }
    declaringComponent
  }

  def allocateAllBindingSlots(pack: StylesheetPackage): Unit = {
    if (getBody != null && getDeclaringComponent.getDeclaringPackage == pack && packageData.isXSLT) {
      allocateBindingSlotsRecursive(pack,
        this,
        getBody,
        getDeclaringComponent.getComponentBindings)
    }
  }

  private def processComponentReference(pack: StylesheetPackage,
                                        invocation: ComponentInvocation,
                                        bindings: List[ComponentBinding]): Unit = {
    val name: SymbolicName = invocation.getSymbolicName
    if (name == null)
      return
    var target: Component = pack.getComponent(name)
    if (target == null &&
      name.getComponentName.hasURI(NamespaceConstant.XSLT) &&
      name.getComponentName.getLocalPart.==("original")) {
      target = pack.getOverriddenComponent(getSymbolicName)
    }
    if (target == null)
      throw new AssertionError("Target of component reference " + name + " is undefined")
    if (invocation.getBindingSlot >= 0)
      throw new AssertionError("**** Component reference " + name + " is already bound")
    val slot: Int = bindings.size
    val cb: ComponentBinding = new ComponentBinding(name, target)
    bindings.add(cb)
    invocation.setBindingSlot(slot)
  }

  def setBody(body: Expression): Unit = {
    this.body = body
    if (body != null)
      body.setParentExpression(null)
  }

  def getBody: Expression = body
  def getChildExpression: Expression = getBody
  def getLocation: Location = this.asInstanceOf[Location]
  def getColumnNumber: Int = -1
  def getPublicId: String = null
  def saveLocation: Location = this.asInstanceOf[Location]
  def getProperty(name: String): AnyRef = null

  def getProperties: Iterator[String] = {
    val list: List[String] = Collections.emptyList()
    list.iterator
  }

  def export(presenter: ExpressionPresenter): Unit
  def isExportable: Boolean = true

  def setChildExpression(expr: Expression): Unit =
    this.body = expr

  def getStackFrameMap: SlotManager = stackFrameMap

  def setStackFrameMap(value: SlotManager): Unit =
    stackFrameMap = value
}
