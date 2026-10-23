package org.orbeon.saxon

import org.orbeon.saxon.expr.Expression.ITERATE_METHOD
import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.instruct.SlotManager
import org.orbeon.saxon.expr.parser.RebindingMap
import org.orbeon.saxon.functions.FunctionLibrary
import org.orbeon.saxon.model.{AnyItemType, ItemType}
import org.orbeon.saxon.om.{NamespaceResolver, Sequence, SequenceIterator, StructuredQName}
import org.orbeon.saxon.s9api.{HostLanguage, Location}
import org.orbeon.saxon.sxpath.{AbstractStaticContext, XPathStaticContext, XPathVariable}
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value.QNameValue

import _root_.java.{util => ju}
import scala.collection.mutable
import scala.jdk.CollectionConverters._

// Similar to Saxon JAXPXPathStaticContext. JAXPXPathStaticContext holds a reference to an XPathVariableResolver, which
// is not desirable as variable resolution occurs at runtime. So here instead we create a fully shareable context.
// Updated to take https://saxonica.plan.io/issues/2554 into account for Saxon 10.
class ShareableXPathStaticContext(
  config           : Configuration,
  functionLibrary  : FunctionLibrary
) extends AbstractStaticContext
     with XPathStaticContext { // only so we can use `sxpath.XPathEvaluator`

  type VariableResolver = (om.StructuredQName, XPathContext) => om.GroundedValue

  // Primary constructor
  locally {
    setConfiguration(config) // also sets `defaultCollationName`

    this.packageData = {
      val pd = new PackageData(config)
      pd.hostLanguage = HostLanguage.XPATH
      pd.setSchemaAware(false)
      pd
    }

    this.usingDefaultFunctionLibrary = true
  }

  // Add function library
  setDefaultFunctionLibrary()
//  getFunctionLibrary.asInstanceOf[FunctionLibraryList].libraryList.add(0, functionLibrary)

  private val variables = mutable.HashMap[StructuredQName, Expression]()
  def referencedVariables: Iterable[StructuredQName] = variables.keys

  // NOTE: We do the same that `IndependentContext` does, to make sure only one `LocalBinding` is created.
  // It's unclear if it's absolutely necessary but it shouldn't hurt.
  private def declareVariableIfNeeded(qName: StructuredQName): Expression =
    variables.getOrElseUpdate(qName, new OrbeonVariableReference(qName))

  class OrbeonVariableReference(name: StructuredQName) extends Expression with Callable {
    def getImplementationMethod: Int = ITERATE_METHOD
    def getItemType: ItemType = AnyItemType
    def `export`(destination: ExpressionPresenter): Unit = {
      destination.startElement("jaxpVar", this)
      destination.emitAttribute("name", name)
      destination.endElement()
    }
    def computeCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE
    def copy(rebindings: RebindingMap) = new OrbeonVariableReference(name)
    def call(context  : XPathContext, arguments: Array[Sequence]): Sequence = {
//      println(s"xxxx evaluating $name")

      if (context.getController eq null)
        throw new NullPointerException

      val variableResolver =
        context.getController.getUserData(
          classOf[ShareableXPathStaticContext].getName,
          "variableResolver"
        ).asInstanceOf[VariableResolver]

      variableResolver(name, context)
    }

    override def iterate(context: XPathContext): SequenceIterator =
      call(context, null).iterate()

    override def getExpressionName: String = "$" + name.getDisplayName
    override def computeSpecialProperties(): Int = StaticProperty.NO_NODES_NEWLY_CREATED

//    def equals(other: Nothing): Boolean = other.isInstanceOf[JAXPVariableReference] && other.asInstanceOf[JAXPVariableReference].name.equals(name
//      ) && (other.asInstanceOf[JAXPVariableReference].resolver eq resolver)
  }

  def bindVariable(qName: StructuredQName): Expression = {
//    println(s"xxxx bindVariable called for $qName, ${variables.size}")
    declareVariableIfNeeded(qName)
//    new OrbeonVariableReference(declareVariableIfNeeded(qName)) {
//      override def evaluateVariable(c: XPathContext): Sequence = {
//        println(s"xxxx evaluateVariable override called for $qName")
//        super.evaluateVariable(c)
//      }
//    }
  }

  // Per Saxon: "used to represent the run-time properties and methods associated with a variable: specifically, a
  // method to get the value of the variable".
//  class VariableBinding(qName: StructuredQName) extends LocalBinding {
//
//    def isGlobal = false // Saxon's `XPathVariable` also uses `false`
//    def isAssignable = false
//
//    private var slotNumber: Int = -1
//    def getLocalSlotNumber: Int = slotNumber
//    def setLocalSlotNumber(slot: Int): Unit = slotNumber = slot
//
//    def getVariableQName: StructuredQName = qName
//    def getRequiredType: SequenceType = SequenceType.ANY_SEQUENCE
//
//    // Saxon does something similar but different in `XPathVariable`, where it stores variables in the the dynamic
//    // context. That uses slots however, which means we cannot resolve variables fully dynamically. So I think
//    // our approach below is ok.
//    def evaluateVariable(context: XPathContext): Sequence = {
//
//      println(s"xxxx evaluating $qName")
//
//      if (context.getController eq null)
//        throw new NullPointerException
//
//      val variableResolver =
//        context.getController.getUserData(
//          classOf[ShareableXPathStaticContext].getName,
//          "variableResolver"
//        ).asInstanceOf[VariableResolver]
//
//      variableResolver(qName, context)
//    }
//
//    // Same as in `XPathVariable`
//    def addReference(ref: VariableReference, isLoopingReference: Boolean): Unit = ()
//    def getIntegerBoundsForVariable: Array[IntegerValue] = null
//    def setIndexedVariable(): Unit = ()
//    def isIndexedVariable: Boolean = false
//  }

  private var namespaces: Map[String, String] = Map.empty

  // For tests
  def declareNamespace(prefix: String, uri: String): Unit =
    namespaces += prefix -> uri

  // Namespace resolver
  object NSResolver extends NamespaceResolver {
    def getURIForPrefix(prefix: String, useDefault: Boolean): String =
      if (prefix == "") {
        if (useDefault)
          getDefaultElementNamespace
        else
          ""
      } else {
        namespaces.getOrElse(prefix, null)
      }

    def iteratePrefixes: ju.Iterator[String] =
      namespaces.keysIterator.asJava
  }

  def getNamespaceResolver: NamespaceResolver = NSResolver

  // Schema stuff which we don't support
  def isImportedSchema(namespace: String): Boolean = getConfiguration.isSchemaAvailable(namespace)
  def getImportedSchemaNamespaces: ju.Set[String] = getConfiguration.getImportedNamespaces

  override def issueWarning(s: String, locator: Location): Unit =
    println(s)

  // Probably unused
  def setNamespaceResolver(resolver: NamespaceResolver): Unit = ???
  def declareVariable(qname: QNameValue): XPathVariable = ???
  def declareVariable(namespaceURI: String, localName   : String): XPathVariable = ???
  val getStackFrameMap: SlotManager = config.makeSlotManager
  def isContextItemParentless: Boolean = false
}
