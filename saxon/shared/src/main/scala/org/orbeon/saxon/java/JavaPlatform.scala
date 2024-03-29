package org.orbeon.saxon.java

import org.orbeon.saxon.event.PipelineConfiguration
import org.orbeon.saxon.functions.FunctionLibraryList
import org.orbeon.saxon.lib.{ModuleURIResolver, StandardModuleURIResolver, StringCollator}
import org.orbeon.saxon.model.ExternalObjectType
import org.orbeon.saxon.regex.{ARegularExpression, JavaRegularExpression, RegularExpression}
import org.orbeon.saxon.resource.StandardCollectionFinder
import org.orbeon.saxon.utils.{Configuration, Platform}
import org.xml.sax.XMLReader

import java.{util => ju}
import javax.xml.transform.Source
import javax.xml.transform.stream.StreamSource


object JavaPlatform {
  var tryJdk9: Boolean = true
}

class JavaPlatform extends Platform {

//  def JAXPStaticContextCheck(retainedStaticContext: RetainedStaticContext,
//                             sc: StaticContext): Boolean = {
//    if (sc.isInstanceOf[JAXPXPathStaticContext] &&
//      !(sc
//        .asInstanceOf[JAXPXPathStaticContext]
//        .getNamespaceContext
//        .isInstanceOf[NamespaceResolver])) {
//      setNamespacesFromJAXP(retainedStaticContext,
//        sc.asInstanceOf[JAXPXPathStaticContext])
//      return true
//    }
//    false
//  }
//
//  private def setNamespacesFromJAXP(
//                                     retainedStaticContext: RetainedStaticContext,
//                                     sc: JAXPXPathStaticContext): Unit = {
//    val nc: NamespaceContext = sc.getNamespaceContext
//    retainedStaticContext.setNamespaces(new NamespaceResolver() {
//      override def getURIForPrefix(prefix: String,
//                                   useDefault: Boolean): String =
//        nc.getNamespaceURI(prefix)
//
//      override def iteratePrefixes: Iterator[String] =
//        throw new UnsupportedOperationException
//    })
//  }

  def initialize(config: Configuration): Unit = {
//    config.registerExternalObjectModel(DOMEnvelope.getInstance)
//    config.registerExternalObjectModel(DOMObjectModel.getInstance)
    config.setCollectionFinder(new StandardCollectionFinder())
  }

  def isJava: Boolean = true

  def isDotNet: Boolean = false

  def getPlatformVersion: String =
    "Java version " + System.getProperty("java.version")

  def getPlatformSuffix: String = "J"

  // ORBEON: TODO: Custom parser loading.
  def loadParser(): XMLReader = {
    ???
//    SAXParserFactory.newInstance().newSAXParser().getXMLReader
  }

  // ORBEON: TODO: Custom parser loading.
  def loadParserForXmlFragments(): XMLReader = {
    ???
//    var factory: SAXParserFactory = null
//    if (tryJdk9) {
//      try {
//        val method: Method =
//          classOf[SAXParserFactory].getMethod("newDefaultInstance")
//        val result: AnyRef = method.invoke(null)
//        factory = result.asInstanceOf[SAXParserFactory]
//      } catch {
//        case e: Exception => tryJdk9 = false
//
//      }
//    }
//    if (factory == null) {
//      try {
//        val factoryClass: Class[_] = Class.forName(
//          "com.sun.org.apache.xerces.internal.jaxp.SAXParserFactoryImpl")
//        factory = factoryClass.newInstance().asInstanceOf[SAXParserFactory]
//      } catch {
//        case e2: Exception => {}
//
//      }
//    }
//    if (factory != null) {
//      try factory.newSAXParser().getXMLReader
//      catch {
//        case e: Exception => {}
//
//      }
//    }
//    loadParser()
  }

  def getParserSource(
    pipe          : PipelineConfiguration,
    input         : StreamSource,
    validation    : Int,
    dtdValidation : Boolean
  ): Source = input

  def makeCollation(
    config : Configuration,
    props  : ju.Properties,
    uri    : String
  ): StringCollator =
    JavaCollationFactory.makeCollation(config, uri, props)

  // ORBEON: Collations
//  def canReturnCollationKeys(collation: StringCollator): Boolean =
//    ! collation.isInstanceOf[SimpleCollation] ||
//      collation
//        .asInstanceOf[SimpleCollation]
//        .getComparator
//        .isInstanceOf[Collator]

  // ORBEON: Collations
//  def getCollationKey(namedCollation: SimpleCollation,
//                      value: String): AtomicMatchKey = {
//    val ck: CollationKey = namedCollation.getComparator
//      .asInstanceOf[Collator]
//      .getCollationKey(value)
//    new CollationMatchKey(ck)
//  }

  def hasICUCollator: Boolean = false
  def hasICUNumberer: Boolean = false

  // ORBEON: Collations
//  def makeUcaCollator(uri: String, config: Configuration): StringCollator = {
//    val collator: UcaCollatorUsingJava = new UcaCollatorUsingJava(uri)
//    if ("yes" == collator.getProperties.getProperty("numeric")) {
//      new AlphanumericCollator(collator)
//    } else {
//      collator
//    }
//  }

  def addFunctionLibraries(
    list         : FunctionLibraryList,
    config       : Configuration,
    hostLanguage : Int
  ): Unit = ()

  def getExternalObjectType(
    config    : Configuration,
    uri       : String,
    localName : String
  ): ExternalObjectType =
    throw new UnsupportedOperationException("getExternalObjectType for Java")

  def getInstallationDirectory(
    edition : String,
    config  : Configuration
  ): String =
    try
      System.getenv("SAXON_HOME")
    catch {
      case _: SecurityException => null
    }

  def registerAllBuiltInObjectModels(config: Configuration): Unit = ()

  def setDefaultSAXParserFactory(config: Configuration): Unit = ()

  def makeStandardModuleURIResolver(config: Configuration): ModuleURIResolver =
    new StandardModuleURIResolver(config)

  def compileRegularExpression(
    config       : Configuration,
    regex        : CharSequence,
    flags        : String,
    hostLanguage : String,
    warnings     : ju.List[String]
  ): RegularExpression =
    if (flags.contains("!")) {
      new JavaRegularExpression(regex, flags.replace("!", ""))
    } else {
      var useJava  = false
      var useSaxon = false

      var flgs     = flags
      val semi = flgs.indexOf(';')
      if (semi >= 0) {
        useJava = flgs.indexOf('j', semi) >= 0
        useSaxon = flgs.indexOf('s', semi) >= 0
        flgs = flgs.substring(0, semi)
      }
      if ("J" == config.getDefaultRegexEngine && ! useSaxon)
        useJava = true
      if (useJava)
        new JavaRegularExpression(regex, flgs)
      else
        new ARegularExpression(regex, flgs, hostLanguage, warnings, config)
    }
}
