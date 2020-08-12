////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.Loc

import net.sf.saxon.expr.parser.RoleDiagnostic

import net.sf.saxon.lib.ConversionRules

import net.sf.saxon.ma.map.MapItem

import net.sf.saxon.model.AtomicType

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.model.Converter

import net.sf.saxon.model.TypeHierarchy

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.trans.Err

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.QNameValue

import net.sf.saxon.value.SequenceType

import net.sf.saxon.value.StringValue

import java.util._

import scala.beans.{BeanProperty, BooleanBeanProperty}

import scala.jdk.CollectionConverters._


/**
 * This class implements the rules for options parameters, as used in functions such as parse-json, serialize,
 * json-to-XML, map:merge. It provides a convenient way of ensuring that the options parameter conventions
 * are enforced.
 */
class OptionsParameter {

  private var allowedOptions: Map[String, SequenceType] = new HashMap(8)

  private var defaultValues: Map[String, Sequence] = new HashMap(8)

  private var requiredOptions: Set[String] = new HashSet(4)

  private var allowedValues: Map[String, Set[String]] = new HashMap(8)

  private var errorCodeForDisallowedValue: String = _

  @BeanProperty
  var errorCodeForAbsentValue: String = "SXJE9999"

  @BooleanBeanProperty
  var allowCastFromString: Boolean = false

  /**
   * Register a permitted option keyword, and the associated type of value, without defining
   * a default value
   *
   * @param name the option keyword
   * @param type the required type
   */
  def addAllowedOption(name: String, `type`: SequenceType): Unit = {
    allowedOptions.put(name, `type`)
  }

  /**
   * Register a required option keyword, and the associated type of value, where omitting
   * the value is an error
   *
   * @param name the option keyword
   * @param type the required type
   */
  def addRequiredOption(name: String, `type`: SequenceType): Unit = {
    allowedOptions.put(name, `type`)
    requiredOptions.add(name)
  }

  /**
   * Register a permitted option keyword, and the associated type of value, with a default value
   *
   * @param name         the option keyword
   * @param type         the required type
   * @param defaultValue the default value if the option is not specified; or null
   *                     if no default is defined
   */
  def addAllowedOption(name: String,
                       `type`: SequenceType,
                       defaultValue: Sequence): Unit = {
    allowedOptions.put(name, `type`)
    if (defaultValue != null) {
      defaultValues.put(name, defaultValue)
    }
  }

  def setAllowedValues(name: String,
                       errorCode: String,
                       values: String*): Unit = {
    val valueSet: HashSet[String] =
      new HashSet[String](Arrays.asList(values: _*))
    allowedValues.put(name, valueSet)
    errorCodeForDisallowedValue = errorCode
  }

  def processSuppliedOptions(supplied: MapItem,
                             context: XPathContext): Map[String, Sequence] = {
    val result: Map[String, Sequence] = new HashMap[String, Sequence]()
    val th: TypeHierarchy = context.getConfiguration.getTypeHierarchy
    for (req <- requiredOptions.asScala
         if supplied.get(new StringValue(req)) == null) {
      throw new XPathException("No value supplied for required option: " + req,
        errorCodeForAbsentValue)
    }
    for ((key, value) <- allowedOptions.asScala) {
      val nominalKey: String = key
      var actualKey: AtomicValue = null
      actualKey =
        if (nominalKey.startsWith("Q{"))
          new QNameValue(StructuredQName.fromEQName(nominalKey),
            BuiltInAtomicType.QNAME)
        else new StringValue(nominalKey)
      val required: SequenceType = value
      var actual: Sequence = supplied.get(actualKey)
      if (actual != null) {
        if (!required.matches(actual, th)) {
          var ok: Boolean = false
          if (actual.isInstanceOf[StringValue] && allowCastFromString &&
            required.getPrimaryType.isInstanceOf[AtomicType]) {
            try {
              val rules: ConversionRules =
                context.getConfiguration.getConversionRules
              actual = Converter.convert(
                actual.asInstanceOf[StringValue],
                required.getPrimaryType.asInstanceOf[AtomicType],
                rules)
              ok = true
            } catch {
              case err: XPathException => ok = false

            }
          }
          if (!ok) {
            val role: RoleDiagnostic =
              new RoleDiagnostic(RoleDiagnostic.OPTION, nominalKey, 0)
            role.setErrorCode("XPTY0004")
            actual =
              th.applyFunctionConversionRules(actual, required, role, Loc.NONE)
          }
        }
        actual = actual.materialize()
        val permitted: Set[String] = allowedValues.get(nominalKey)
        if (permitted != null) {
          if (!(actual.isInstanceOf[AtomicValue]) ||
            !permitted.contains(
              actual.asInstanceOf[AtomicValue].getStringValue)) {
            val message: StringBuilder = new StringBuilder(
              "Invalid option " + nominalKey + "=" + Err
                .depictSequence(actual) +
                ". Valid values are:")
            var i: Int = 0
            for (v <- permitted.asScala) {
              message
                .append(if ( {
                  i += 1; i - 1
                } == 0) " " else ", ")
                .append(v)
            }
            throw new XPathException(message.toString,
              errorCodeForDisallowedValue)
          }
        }
        result.put(nominalKey, actual)
      } else {
        val `def`: Sequence = defaultValues.get(nominalKey)
        if (`def` != null) {
          result.put(nominalKey, `def`)
        }
      }
    }
    result
  }

  def getDefaultOptions(): Map[String, Sequence] = {
    val result: Map[String, Sequence] = new HashMap[String, Sequence]()
    for ((key, value) <- defaultValues.asScala) {
      result.put(key, value)
    }
    result
  }

}
