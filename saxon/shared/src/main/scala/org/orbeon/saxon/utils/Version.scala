package org.orbeon.saxon.utils

import org.orbeon.saxon.saxjava.JavaPlatform

object Version {

  private val MAJOR_VERSION: Int = 10
  private val MINOR_VERSION: Int = 1

  //mddhh
  private val BUILD: Int = 51412
  private val MAJOR_RELEASE_DATE: String = "2020-03-16"
  private val MINOR_RELEASE_DATE: String = "2020-05-14"

  def getProductName: String = "SAXON"
  def getProductVendor: String = "Saxonica"

  def getProductVariantAndVersion(edition: String): String =
    edition + " " + getProductVersion

  def getProductVersion: String = MAJOR_VERSION.toString + "." + MINOR_VERSION.toString

  def getStructuredVersionNumber: Array[Int] =
    Array(MAJOR_VERSION, MINOR_VERSION, BUILD)

  def getReleaseDate: String = MINOR_RELEASE_DATE
  def getMajorReleaseDate: String = MAJOR_RELEASE_DATE

  def getProductTitle: String =
    getProductName + '-' + softwareEdition + ' ' + getProductVersion +
      (if (platform.isJava) 'J' else 'N') +
      " from Saxonica"

  def getWebSiteAddress: String = "http://www.saxonica.com/"

  /**
   * Invoking org.orbeon.saxon.Version from the command line outputs the build number
   *
   * @param args not used
   */
  def main(args: Array[String]): Unit =
    System.err.println(getProductTitle + " (build " + BUILD + ')')

  var configurationClass: Class[_ <: Configuration] = _
  var softwareEdition: String = "HE"
  var platform: Platform = new JavaPlatform()
}
