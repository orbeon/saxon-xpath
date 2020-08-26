package net.sf.saxon.saxjava

import net.sf.saxon.dom.DOMEnvelope
import net.sf.saxon.dom.DOMObjectModel
import net.sf.saxon.event.PipelineConfiguration
import net.sf.saxon.expr.StaticContext
import net.sf.saxon.expr.parser.RetainedStaticContext
import net.sf.saxon.expr.sort._
import net.sf.saxon.functions.FunctionLibraryList
import net.sf.saxon.lib.ModuleURIResolver
import net.sf.saxon.lib.StandardModuleURIResolver
import net.sf.saxon.lib.StringCollator
import net.sf.saxon.model.ExternalObjectType
import net.sf.saxon.om.NamespaceResolver
import net.sf.saxon.regex.ARegularExpression
import net.sf.saxon.regex.JavaRegularExpression
import net.sf.saxon.regex.RegularExpression
import net.sf.saxon.resource.StandardCollectionFinder
import net.sf.saxon.trans.XPathException
import net.sf.saxon.xpath.JAXPXPathStaticContext
import org.xml.sax.SAXException
import org.xml.sax.XMLReader
import javax.xml.namespace.NamespaceContext
import javax.xml.parsers.ParserConfigurationException
import javax.xml.parsers.SAXParserFactory
import javax.xml.transform.Source
import javax.xml.transform.TransformerFactoryConfigurationError
import javax.xml.transform.stream.StreamSource
import java.lang.reflect.Method
import java.text.CollationKey
import java.text.Collator
import java.util.Iterator
import java.util.List
import java.util.Properties

import JavaPlatform._
import net.sf.saxon.javasax.JavaCollationFactory
import net.sf.saxon.utils.{Configuration, Platform}

object JavaPlatform {
  var tryJdk9: Boolean = true
}

class JavaPlatform extends Platform {

  def JAXPStaticContextCheck(retainedStaticContext: RetainedStaticContext,
                             sc: StaticContext): Boolean = {
    if (sc.isInstanceOf[JAXPXPathStaticContext] &&
      !(sc
        .asInstanceOf[JAXPXPathStaticContext]
        .getNamespaceContext
        .isInstanceOf[NamespaceResolver])) {
      setNamespacesFromJAXP(retainedStaticContext,
        sc.asInstanceOf[JAXPXPathStaticContext])
      return true
    }
    false
  }

  private def setNamespacesFromJAXP(
                                     retainedStaticContext: RetainedStaticContext,
                                     sc: JAXPXPathStaticContext): Unit = {
    val nc: NamespaceContext = sc.getNamespaceContext
    retainedStaticContext.setNamespaces(new NamespaceResolver() {
      override def getURIForPrefix(prefix: String,
                                   useDefault: Boolean): String =
        nc.getNamespaceURI(prefix)

      override def iteratePrefixes(): Iterator[String] =
        throw new UnsupportedOperationException()
    })
  }

  def initialize(config: Configuration): Unit = {
    config.registerExternalObjectModel(DOMEnvelope.getInstance)
    config.registerExternalObjectModel(DOMObjectModel.getInstance)
    config.setCollectionFinder(new StandardCollectionFinder())
  }

  def isJava(): Boolean = true

  def isDotNet(): Boolean = false

  def getPlatformVersion(): String =
    "Java version " + System.getProperty("java.version")

  def getPlatformSuffix(): String = "J"

  def loadParser(): XMLReader = {
    var parser: XMLReader = null
    parser = SAXParserFactory.newInstance().newSAXParser().getXMLReader
    parser
  }

  def loadParserForXmlFragments(): XMLReader = {
    var factory: SAXParserFactory = null
    if (tryJdk9) {
      try {
        val method: Method =
          classOf[SAXParserFactory].getMethod("newDefaultInstance")
        val result: AnyRef = method.invoke(null)
        factory = result.asInstanceOf[SAXParserFactory]
      } catch {
        case e: Exception => tryJdk9 = false

      }
    }
    if (factory == null) {
      try {
        val factoryClass: Class[_] = Class.forName(
          "com.sun.org.apache.xerces.internal.jaxp.SAXParserFactoryImpl")
        factory = factoryClass.newInstance().asInstanceOf[SAXParserFactory]
      } catch {
        case e2: Exception => {}

      }
    }
    if (factory != null) {
      try factory.newSAXParser().getXMLReader
      catch {
        case e: Exception => {}

      }
    }
    loadParser()
  }

  def getParserSource(pipe: PipelineConfiguration,
                      input: StreamSource,
                      validation: Int,
                      dtdValidation: Boolean): Source = input

  def makeCollation(config: Configuration,
                    props: Properties,
                    uri: String): StringCollator =
    JavaCollationFactory.makeCollation(config, uri, props)

  def canReturnCollationKeys(collation: StringCollator): Boolean =
    !(collation.isInstanceOf[SimpleCollation]) ||
      collation
        .asInstanceOf[SimpleCollation]
        .getComparator
        .isInstanceOf[Collator]

  def getCollationKey(namedCollation: SimpleCollation,
                      value: String): AtomicMatchKey = {
    val ck: CollationKey = namedCollation.getComparator
      .asInstanceOf[Collator]
      .getCollationKey(value)
    new CollationMatchKey(ck)
  }

  def hasICUCollator(): Boolean = false

  def hasICUNumberer(): Boolean = false

  def makeUcaCollator(uri: String, config: Configuration): StringCollator = {
    val collator: UcaCollatorUsingJava = new UcaCollatorUsingJava(uri)
    if ("yes" == collator.getProperties.getProperty("numeric")) {
      new AlphanumericCollator(collator)
    } else {
      collator
    }
  }

  def addFunctionLibraries(list: FunctionLibraryList,
                           config: Configuration,
                           hostLanguage: Int): Unit = ()

  def getExternalObjectType(config: Configuration,
                            uri: String,
                            localName: String): ExternalObjectType =
    throw new UnsupportedOperationException("getExternalObjectType for Java")

  def getInstallationDirectory(edition: String,
                               config: Configuration): String =
    try System.getenv("SAXON_HOME")
    catch {
      case e: SecurityException => null

    }

  def registerAllBuiltInObjectModels(config: Configuration): Unit = ()

  def setDefaultSAXParserFactory(config: Configuration): Unit = ()

  def makeStandardModuleURIResolver(config: Configuration): ModuleURIResolver =
    new StandardModuleURIResolver(config)

  override def compileRegularExpression(config: Configuration, regex: CharSequence, flags: String, hostLanguage: String, warnings: scala.List[String]): RegularExpression =
    if (flags.contains("!")) {
      new JavaRegularExpression(regex, flags.replace("!", ""))
    } else {
      var useJava: Boolean = false
      var useSaxon: Boolean = false
      var flgs = flags
      val semi: Int = flgs.indexOf(';')
      if (semi >= 0) {
        useJava = flgs.indexOf('j', semi) >= 0
        useSaxon = flgs.indexOf('s', semi) >= 0
        flgs = flgs.substring(0, semi)
      }
      if ("J" == config.getDefaultRegexEngine && !useSaxon) {
        useJava = true
      }
      if (useJava) {
        new JavaRegularExpression(regex, flgs)
      } else {
        new ARegularExpression(regex, flgs, hostLanguage, warnings.asInstanceOf[List[String]], config)
      }
    }
}
