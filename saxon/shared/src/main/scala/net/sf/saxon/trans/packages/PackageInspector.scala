package net.sf.saxon.trans.packages

import java.io.File

import javax.xml.transform.stream.StreamSource
import net.sf.saxon.event.{PipelineConfiguration, ProxyReceiver, Sender, Sink}
import net.sf.saxon.lib.{NamespaceConstant, ParseOptions, Validation}
import net.sf.saxon.model.SchemaType
import net.sf.saxon.om.{AttributeInfo, AttributeMap, NamespaceMap, NodeName}
import net.sf.saxon.s9api.Location
import net.sf.saxon.trans.XPathException
import net.sf.saxon.utils.{Configuration, Version}

//remove if not needed
// import scala.collection.JavaConversions._

object PackageInspector {

  def getPackageDetails(top: File, config: Configuration): PackageDetails = {
    val inspector: PackageInspector = new PackageInspector(
      config.makePipelineConfiguration)
    try {
      val options: ParseOptions = new ParseOptions()
      options.setDTDValidationMode(Validation.SKIP)
      options.setSchemaValidationMode(Validation.SKIP)
      Sender.send(new StreamSource(top), inspector, new ParseOptions())
    } catch {
      case e: XPathException =>
        if (e.getMessage.!=("#start#")) {
          throw e
        }

    }
    val vp: VersionedPackageName = inspector.getNameAndVersion
    if (vp == null) {
      null
    } else {
      val details: PackageDetails = new PackageDetails()
      details.nameAndVersion = vp
      if (inspector.isSefFile) {
        details.exportLocation = new StreamSource(top)
      } else {
        details.sourceLocation = new StreamSource(top)
      }
      details
    }
  }

}

class PackageInspector private (pipe: PipelineConfiguration)
  extends ProxyReceiver(new Sink(pipe)) {

  private var isSefFile: Boolean = _

  private var packageName: String = _

  private var packageVersion: String = "1"

  private var elementCount: Int = 0

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    if ({ elementCount += 1; elementCount - 1 } >= 1) {
      throw new XPathException("#start#")
    }
    isSefFile = elemName.hasURI(NamespaceConstant.SAXON_XSLT_EXPORT)
    if (attributes.get("", "name") != null) {
      packageName = attributes.get("", "name").getValue
    }
    if (attributes.get("", "package-version") != null) {
      packageVersion = attributes.get("", "package-version").getValue
    }
    if (attributes.get("", "packageVersion") != null) {
      packageVersion = attributes.get("", "packageVersion").getValue
    }
    val saxonVersion: AttributeInfo = attributes.get("", "saxonVersion")
    if (saxonVersion != null) {
      if (saxonVersion.getValue.startsWith("9")) {
        throw new XPathException(
          "Saxon " + Version.getProductVersion + " cannot load a SEF file created using version " +
            saxonVersion.getValue)
      }
    }
  }

  private def getNameAndVersion(): VersionedPackageName = {
    if (packageName == null) {
      null
    }
    try new VersionedPackageName(packageName, packageVersion)
    catch {
      case e: XPathException => null

    }
  }

}