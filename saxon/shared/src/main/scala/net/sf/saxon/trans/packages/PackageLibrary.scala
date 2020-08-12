package net.sf.saxon.trans.packages

import net.sf.saxon.utils.Configuration

import net.sf.saxon.om.GroundedValue

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.style._

import net.sf.saxon.trans.CompilerInfo

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.FastStringBuffer

import javax.xml.transform.Source

import java.io.File

import java.util._

import scala.beans.{BeanProperty, BooleanBeanProperty}

import scala.jdk.CollectionConverters._

class PackageLibrary {

  @BeanProperty var compilerInfo: CompilerInfo = _

  private var config: Configuration = compilerInfo.getConfiguration

  /*  private var packageVersions: Map[String, List[PackageVersion]] =
      new HashMap()*/

  private var packages: Map[VersionedPackageName, PackageDetails] =
    new HashMap()

  def this(compilerInfo: CompilerInfo) {
    this()
    this.compilerInfo = compilerInfo;
    this.config = compilerInfo.getConfiguration();
  }

  def this(library: PackageLibrary) = {
    this()
    /*packageVersions = new HashMap(library.packageVersions)*/
    packages = new HashMap(library.packages)
    compilerInfo = library.compilerInfo
    config = library.config
  }

  def this(info: CompilerInfo, files: Set[File]) = {
    this()
    compilerInfo = info
    config = info.getConfiguration
    for (file <- files.asScala) {
      val details: PackageDetails =
        PackageInspector.getPackageDetails(file, config)
      if (details == null) {
        throw new XPathException(
          "Unable to get package name and version for file " + file.getName)
      }
      addPackage(details)
    }
  }

  def addPackage(packageIn: StylesheetPackage): Unit = {
    synchronized {
      val name: String = packageIn.getPackageName
      // val version: PackageVersion = packageIn.getPackageVersion
      val vp: VersionedPackageName = new VersionedPackageName(name, null)
      val details: PackageDetails = new PackageDetails()
      details.nameAndVersion = vp
      details.loadedPackage = packageIn
      packages.put(vp, details)
      addPackage(details)
    }
  }

  def addPackage(details: PackageDetails): Unit = {
    synchronized {
      val vp: VersionedPackageName = details.nameAndVersion
      val name: String = vp.packageName
      /* val version: PackageVersion = vp.packageVersion
       val versions: List[PackageVersion] =
         packageVersions.computeIfAbsent(name, (k) => new ArrayList())*/
      //versions.add(version)
      packages.put(vp, details)
    }
  }

  def addPackage(file: File): Unit = {
    val details: PackageDetails =
      PackageInspector.getPackageDetails(file, config)
    if (details == null) {
      throw new XPathException(
        "Unable to get package name and version for file " + file.getName)
    }
    addPackage(details)
  }

  def findPackage(name: String /*, ranges: PackageVersionRanges*/): PackageDetails = synchronized {
    val candidates: Set[PackageDetails] = new HashSet[PackageDetails]()
    /*  val available: List[PackageVersion] = packageVersions.get(name)
      if (available == null) {
        null
      }*/
    var maxPriority: Int = java.lang.Integer.MIN_VALUE
    /*  for (pv <- available) {
        val details: PackageDetails =
          packages.get(new VersionedPackageName(name, pv))
        if (ranges.contains(pv)) {
          candidates.add(details)
          val priority: java.lang.Integer = details.priority
          if (priority != null && priority > maxPriority) {
            maxPriority = priority
          }
        }
      }*/
    if (candidates.isEmpty) {
      return null
    } else if (candidates.size == 1) {
      candidates.iterator().next()
    } else {
      // val shortList: Set[PackageVersion] = new HashSet[PackageVersion]()
      var highest: PackageDetails = null
      /* if (maxPriority == java.lang.Integer.MIN_VALUE) {
         for (details <- candidates.asScala if highest == null ||
           details.nameAndVersion.packageVersion.compareTo(
             highest.nameAndVersion.packageVersion) >
             0) {
           highest = details
         }
       } else {
         for (details <- candidates.asScala) {
           val priority: java.lang.Integer = details.priority
           val pv: PackageVersion = details.nameAndVersion.packageVersion
           if (priority != null && priority == maxPriority &&
             (highest == null ||
               pv.compareTo(highest.nameAndVersion.packageVersion) >
                 0)) {
             highest = details
           }
         }*/
      highest
    }
  }

  def findDetailsForAlias(shortName: String): PackageDetails = synchronized {
    assert(shortName != null)
    var selected: PackageDetails = null
    for (details <- packages.values.asScala if shortName == details.shortName) {
      if (selected == null) {
        selected = details
      } else {
        throw new IllegalStateException(
          "Non-unique shortName in package library: " + shortName)
      }
    }
    selected
  }

  def obtainLoadedPackage(
                           details: PackageDetails,
                           disallowed: List[VersionedPackageName]): StylesheetPackage =
    if (details.loadedPackage != null) {
      details.loadedPackage
    } else if (details.exportLocation != null) {
      testForCycles(details, disallowed)
      details.beingProcessed = Thread.currentThread()
      val input: Source = details.exportLocation
      val loader: IPackageLoader = config.makePackageLoader
      val pack: StylesheetPackage = loader.loadPackage(input)
      checkNameAndVersion(pack, details)
      details.loadedPackage = pack
      details.beingProcessed = null
      pack
    } else if (details.sourceLocation != null) {
      testForCycles(details, disallowed)
      details.beingProcessed = Thread.currentThread()
      /*val compilation: Compilation = new Compilation(config, compilerInfo)
      compilation.setUsingPackages(disallowed)
      compilation.setExpectedNameAndVersion(details.nameAndVersion)
      compilation.clearParameters()
      compilation.setLibraryPackage(true)*/
      /*if (details.staticParams != null) {
        for ((key, value) <- details.staticParams) {
          compilation.setParameter(key, value)
        }
      }*/
      /*   val psm: PrincipalStylesheetModule =
           compilation.compilePackage(details.sourceLocation)
         details.beingProcessed = null
         if (compilation.getErrorCount > 0) {
           throw new XPathException(
             "Errors found in package " + details.nameAndVersion.packageName)
         }*/
      //val styPack: StylesheetPackage = psm.getStylesheetPackage
      /*  checkNameAndVersion(styPack, details)
        details.loadedPackage = styPack
        styPack*/
      new StylesheetPackage(new Configuration)
    } else {
      null
    }

  private def testForCycles(details: PackageDetails,
                            disallowed: List[VersionedPackageName]): Unit = {
    if (details.beingProcessed == Thread.currentThread()) {
      val buffer: FastStringBuffer = new FastStringBuffer(1024)
      for (n <- disallowed.asScala) {
        buffer.append(n.packageName)
        buffer.append(", ")
      }
      buffer.append("and ")
      buffer.append(details.nameAndVersion.packageName)
      throw new XPathException(
        "There is a cycle of package dependencies involving " +
          buffer,
        "XTSE3005")
    }
  }

  private def checkNameAndVersion(pack: StylesheetPackage,
                                  details: PackageDetails): Unit = {
    val storedName: String = pack.getPackageName
    if (details.baseName != null) {
      if (details.baseName != storedName) {
        throw new XPathException(
          "Base name of package (" + details.baseName + ") does not match the value in the XSLT source (" +
            storedName +
            ")")
      }
    } else {
      if (details.nameAndVersion.packageName != storedName) {
        throw new XPathException(
          "Registered name of package (" + details.nameAndVersion.packageName +
            ") does not match the value in the XSLT source (" +
            storedName +
            ")")
      }
    }
    //val actualVersion: PackageVersion = pack.getPackageVersion
    /*if (actualVersion != details.nameAndVersion.packageVersion) {
      throw new XPathException(
        "Registered version number of package (" + details.nameAndVersion.packageVersion +
          ") does not match the value in the XSLT source (" +
          actualVersion +
          ")")
    }*/
  }

  def getPackages(): List[StylesheetPackage] = synchronized {
    val result: List[StylesheetPackage] = new ArrayList[StylesheetPackage]()
    for (details <- packages.values.asScala if details.loadedPackage != null) {
      result.add(details.loadedPackage)
    }
    result
  }

}
