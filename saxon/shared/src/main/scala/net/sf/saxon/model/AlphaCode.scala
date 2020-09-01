package net.sf.saxon.model

import net.sf.saxon.utils.Configuration
import net.sf.saxon.expr.StaticProperty
import net.sf.saxon.expr.XPathContext
import net.sf.saxon.expr.parser.Token
import scala.jdk.CollectionConverters._
import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.ma.arrays.ArrayItem
import net.sf.saxon.ma.arrays.ArrayItemType
import net.sf.saxon.ma.arrays.SimpleArrayItem
import net.sf.saxon.ma.map._
import net.sf.saxon.om.GroundedValue
import net.sf.saxon.om.StructuredQName
import net.sf.saxon.pattern._
import net.sf.saxon.sxpath.IndependentContext
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.Cardinality
import net.sf.saxon.value.SequenceExtent
import net.sf.saxon.value.SequenceType
import net.sf.saxon.value.StringValue
import java.util
import scala.util.control.Breaks._
import scala.jdk.CollectionConverters._

object AlphaCode {

  trait ParserCallBack[T] {

    def makeContainer: T


    def setStringProperty(container: T, key: String, value: String): Unit


    def setMultiStringProperty(container: T, key: String, value: util.List[String]): Unit


    def setTypeProperty(container: T, key: String, value: T): Unit


    def setMultiTypeProperty(container: T, key: String, value: util.List[T]): Unit
  }


  private class MapItemCallBack extends AlphaCode.ParserCallBack[DictionaryMap] {
    override def makeContainer = new DictionaryMap

    override def setStringProperty(container: DictionaryMap, key: String, value: String) = container.initialPut(key, new StringValue(value))

    override def setMultiStringProperty(container: DictionaryMap, key: String, value: util.List[String]) = {
      val xdmValue = new util.ArrayList[StringValue]

      for (v <- value.asScala) {
        xdmValue.add(new StringValue(v))
      }
      container.initialPut(key, new SequenceExtent(xdmValue))
    }

    override def setTypeProperty(container: DictionaryMap, key: String, value: DictionaryMap) = container.initialPut(key, value)

    override def setMultiTypeProperty(container: DictionaryMap, key: String, value: util.List[DictionaryMap]) = {
      val contents = new util.ArrayList[GroundedValue](value)
      container.initialPut(key, new SimpleArrayItem(contents))
    }
  }


  private class TreeCallBack extends AlphaCode.ParserCallBack[AlphaCode.AlphaCodeTree] {
    override def makeContainer = new AlphaCode.AlphaCodeTree

    override def setStringProperty(tree: AlphaCode.AlphaCodeTree, key: String, value: String) = key match {
      case "o" =>
        tree.cardinality = value
      case "p" =>
        tree.principal = value
      case "n" =>
        tree.name = value
      case "c" =>
        tree.content = value
      case "z" =>
        tree.nillable = true
      case "x" =>
        tree.extensibleTupleType = true
      case _ =>
        throw new IllegalArgumentException("Bad alphacode component " + key)
    }

    override def setMultiStringProperty(tree: AlphaCode.AlphaCodeTree, key: String, value: util.List[String]) = if (key == "f") {
      tree.fieldNames = value
    }
    else throw new IllegalArgumentException("Bad alphacode component " + key)

    override def setTypeProperty(tree: AlphaCode.AlphaCodeTree, key: String, value: AlphaCode.AlphaCodeTree) = key match {
      case "k" =>
        tree.keyType = value
      case "v" =>
        tree.valueType = value
      case "r" =>
        tree.resultType = value
      case "e" =>
        tree.elementType = value
      case _ =>
        throw new IllegalArgumentException("Bad alphacode component " + key)
    }

    override def setMultiTypeProperty(tree: AlphaCode.AlphaCodeTree, key: String, value: util.List[AlphaCode.AlphaCodeTree]) = key match {
      case "a" =>
        tree.argTypes = value
      case "m" =>
        tree.members = value
      case "i" =>
        tree.vennOperands = value.asScala.toArray
        tree.vennOperator = Token.INTERSECT
      case "u" =>
        tree.vennOperands = value.asScala.toArray
        tree.vennOperator = Token.UNION
      case "d" =>
        tree.vennOperands = value.asScala.toArray
        tree.vennOperator = Token.EXCEPT
      case _ =>
        throw new IllegalArgumentException("Bad alphacode component " + key)
    }
  }


  class AlphaCodeParser[T](var input: String, var callBack: AlphaCode.ParserCallBack[T]) {
    private var position = 0

    private def nextChar: Int = {
      if (position >= input.length) return -1
      input.charAt({
        position += 1;
        position - 1
      })
    }

    private def nextToken: String = {
      var inBraces = 0
      val start = position
      while ( {
        position < input.length
      }) {
        val ch = input.charAt({
          position += 1;
          position - 1
        })
        ch match {
          case '{' =>
            inBraces += 1
          case '}' =>
            inBraces -= 1
          case ']' =>
          case ',' =>
            if (inBraces == 0) return input.substring(start, {
              position -= 1;
              position
            })
          case ' ' =>
            if (inBraces == 0) return input.substring(start, position - 1)
          case _ =>

        }
      }
      input.substring(start, position)
    }

    private def expect(c: Char) = {
      val d = nextChar
      if (d != c) throw new IllegalStateException("Expected '" + c + "', found '" + (if (d == -1) "<eof>"
      else d.toChar) + "'")
    }

    private[model] def parseType: T = {
      val container: T = callBack.makeContainer
      var indicator = nextChar
      if (indicator < 0) callBack.setStringProperty(container, "o", "1")
      else if ("*+1?0\u00B0".indexOf(indicator.toChar) >= 0) {
        if (indicator == 0xB0) indicator = '0'
        callBack.setStringProperty(container, "o", "" + indicator.toChar)
      }
      else {
        callBack.setStringProperty(container, "o", "1")
        position -= 1
      }
      val primary = nextToken
      callBack.setStringProperty(container, "p", primary)
      while (position < input.length) {
        val c = input.charAt(position)
        c match {
          case ']' =>
          case ',' =>
            return container
          case ' ' =>
            position += 1
          case 'n' =>
          case 'c' =>
            position += 1
            var token = nextToken
            if (token.startsWith("~")) token = "Q{" + NamespaceConstant.SCHEMA + "}" + token.substring(1)
            if (c == 'c' && token.endsWith("?")) {
              callBack.setStringProperty(container, "z", "1")
              token = token.substring(0, token.length - 1)
            }
            callBack.setStringProperty(container, "" + c, token)
          case 'k' =>
          case 'r' =>
          case 'v' =>
          case 'e' =>
            position += 1
            expect('[')
            val nestedType = parseType
            expect(']')
            callBack.setTypeProperty(container, "" + c, nestedType)
          case 'a' =>
          case 'm' =>
          case 'i' =>
          case 'u' =>
          case 'd' =>
            position += 1
            expect('[')
            val nestedTypes = new util.ArrayList[T]
            if (input.charAt(position) == ']') {
              position += 1
              callBack.setMultiTypeProperty(container, "" + c, nestedTypes)
            }
            else breakable {
              while (true) {
                nestedTypes.add(parseType)
                if (input.charAt(position) == ',') position += 1
                else {
                  expect(']')
                  callBack.setMultiTypeProperty(container, "" + c, nestedTypes)
                  break()
                }
              }
            }
          case 'f' =>
          case 'F' =>
            if (c == 'F') callBack.setStringProperty(container, "x", "1")
            position += 1
            expect('[')
            val fieldNames = new util.ArrayList[String]
            val currName = new StringBuilder
            var escaped = false
            breakable {
              while (true) {
                position += 1
                val ch = input.charAt(position)
                if (ch == '\\' && !escaped) escaped = true
                else if (ch == ',' && !escaped) {
                  fieldNames.add(currName.toString)
                  currName.setLength(0)
                  escaped = false
                }
                else if (ch == ']' && !escaped) {
                  fieldNames.add(currName.toString)
                  currName.setLength(0)
                  callBack.setMultiStringProperty(container, "f", fieldNames)
                  break()
                }
                else {
                  currName.append(ch)
                  escaped = false
                }
              }
            }
          case _ =>
            throw new IllegalStateException("Expected one of n|c|t|k|r|v|a|u, found '" + c + "'")
        }
      }
      container
    }
  }


  def toXdmMap(input: String) = {
    val callBack = new AlphaCode.MapItemCallBack
    val parser = new AlphaCode.AlphaCodeParser[DictionaryMap](input, callBack)
    parser.parseType
  }

  def fromXdmMap(map: MapItem): String = {
    val out = new StringBuilder
    val indicator = map.get(new StringValue("o")).asInstanceOf[StringValue]
    out.append(if (indicator == null) "1"
    else indicator.getStringValue)
    val alphaCode = map.get(new StringValue("p")).asInstanceOf[StringValue]
    out.append(if (alphaCode == null) ""
    else alphaCode.getStringValue)
    out.append(" ")

    for (kvp <- map.keyValuePairs.asScala) {
      val key = kvp.key.getStringValue
      key match {
        case "o" =>
        case "p" =>
        case "n" =>
        case "c" =>
        case "t" =>
          out.append(key)
          out.append(kvp.value.asInstanceOf[StringValue].getStringValue)
          out.append(" ")
        case "k" =>
        case "r" =>
        case "v" =>
        case "e" =>
          out.append(key)
          out.append('[')
          out.append(fromXdmMap(kvp.value.asInstanceOf[MapItem]))
          out.append(']')
          out.append(" ")
        case "a" =>
        case "u" =>
          out.append(key)
          out.append('[')
          val types = kvp.value.asInstanceOf[ArrayItem]
          var first = true

          for (t <- types.members) {
            if (first) first = false
            else out.append(",")
            out.append(fromXdmMap(t.asInstanceOf[MapItem]))
          }
          out.append(']')
          out.append(" ")
        case _ =>
          throw new IllegalStateException("Unexpected key '" + key + "'")
      }
    }
    out.toString
  }


  private class AlphaCodeTree {
    private[model] var cardinality: String = null
    private[model] var principal: String = null
    private[model] var name: String = null
    private[model] var content: String = null
    private[model] var nillable = false
    private[model] var members: util.List[AlphaCodeTree] = null
    private[model] var keyType: AlphaCodeTree = null
    private[model] var valueType: AlphaCodeTree = null
    private[model] var resultType: AlphaCodeTree = null
    private[model] var argTypes: util.List[AlphaCodeTree] = null
    private[model] var elementType: AlphaCodeTree = null
    private[model] var vennOperator = 0
    private[model] var vennOperands: Array[AlphaCodeTree] = null
    private[model] var fieldNames: util.List[String] = null
    private[model] var extensibleTupleType = false
  }


  def toSequenceType(input: String, config: Configuration) = {
    val callBack = new AlphaCode.TreeCallBack
    val parser = new AlphaCode.AlphaCodeParser[AlphaCode.AlphaCodeTree](input, callBack)
    val tree = parser.parseType
    sequenceTypeFromTree(tree, config)
  }


  def toItemType(input: String, config: Configuration) = {
    val st = toSequenceType(input, config)
    if (st.getCardinality != StaticProperty.EXACTLY_ONE) throw new IllegalArgumentException("Supplied alphacode has a cardinality other than 1")
    st.getPrimaryType
  }


  private def sequenceTypeFromTree(tree: AlphaCodeTree, config: Configuration): SequenceType = {
    val principal = tree.principal
    var itemType: ItemType = null
    if (principal.isEmpty) itemType = AnyItemType
    else if (principal.startsWith("A")) {
      val builtIn = BuiltInAtomicType.fromAlphaCode(principal)
      if (builtIn == null) throw new IllegalArgumentException("Unknown type " + principal)
      itemType = builtIn
      if (tree.name != null) {
        val `type` = config.getSchemaType(StructuredQName.fromEQName(tree.name))
        if (!`type`.isInstanceOf[PlainType]) throw new IllegalArgumentException("Schema type " + tree.name + " is not known")
        itemType = `type`.asInstanceOf[PlainType]
      }
      else if ((builtIn eq BuiltInAtomicType.ANY_ATOMIC) && tree.members != null) {
        val members = new util.ArrayList[AtomicType]

        for (m <- tree.members.asScala) {
          val st = sequenceTypeFromTree(m, config)
          if (st.getPrimaryType.isAtomicType) {
            val primaryType = st.getPrimaryType.asInstanceOf[AtomicType]
            members.add(primaryType)
          }
        }
        itemType = new LocalUnionType(members)
      }
    }
    else if (principal.startsWith("N")) {
      val contentName: String = tree.content
      var contentQName: StructuredQName = null
      var contentTest: ContentTypeTest = null
      val nillable = tree.nillable
      if (contentName != null) {
        contentQName = StructuredQName.fromEQName(contentName)
        val contentType = config.getSchemaType(contentQName)
        if (contentType == null) throw new IllegalArgumentException("Unknown type " + contentName)
        contentTest = new ContentTypeTest(if (principal == "NE") Type.ELEMENT
        else Type.ATTRIBUTE, contentType, config, nillable)
      }
      if (tree.vennOperands != null) if (tree.vennOperands.length == 2) {
        val nt0 = sequenceTypeFromTree(tree.vennOperands(0), config).getPrimaryType.asInstanceOf[NodeTest]
        val nt1 = sequenceTypeFromTree(tree.vennOperands(1), config).getPrimaryType.asInstanceOf[NodeTest]
        itemType = new CombinedNodeTest(nt0, tree.vennOperator, nt1)
      }
      else {
        assert(tree.vennOperator == Token.UNION)
        var u = UType.VOID
        for (i <- 0 until tree.vennOperands.length) {
          val it = sequenceTypeFromTree(tree.vennOperands(i), config).getPrimaryType
          assert(it.isInstanceOf[NodeKindTest])
          u = u.union(it.getUType)
        }
        itemType = new MultipleNodeKindTest(u)
      }
      else {
        var kind = Type.NODE
        if (principal.length >= 2) principal.substring(0, 2) match {
          case "NT" =>
            kind = Type.TEXT
          case "NC" =>
            kind = Type.COMMENT
          case "NN" =>
            kind = Type.NAMESPACE
          case "NP" =>
            kind = Type.PROCESSING_INSTRUCTION
          case "ND" =>
            kind = Type.DOCUMENT
          case "NE" =>
            kind = Type.ELEMENT
          case "NA" =>
            kind = Type.ATTRIBUTE
        }
        val name = tree.name
        var partialNameTest: QNameTest = null
        if (name != null && name.contains("*")) if (name.startsWith("*:")) partialNameTest = new LocalNameTest(config.getNamePool, kind, name.substring(2))
        else if (name.endsWith("}*")) {
          val uri = name.substring(2, name.length - 2)
          partialNameTest = new NamespaceTest(config.getNamePool, kind, uri)
        }
        if (partialNameTest != null) itemType = partialNameTest.asInstanceOf[NodeTest]
        else {
          val qName = if (name == null) null
          else StructuredQName.fromEQName(name)
          principal match {
            case "N" =>
              itemType = AnyNodeTest.getInstance
            case "NT" =>
              itemType = NodeKindTest.TEXT
            case "NC" =>
              itemType = NodeKindTest.COMMENT
            case "NN" =>
              if (name == null) itemType = NodeKindTest.NAMESPACE
              else itemType = new NameTest(Type.NAMESPACE, "", qName.getLocalPart, config.getNamePool)
            case "NP" =>
              if (name == null) itemType = NodeKindTest.PROCESSING_INSTRUCTION
              else itemType = new NameTest(Type.PROCESSING_INSTRUCTION, "", qName.getLocalPart, config.getNamePool)
            case "ND" =>
              val elementType = tree.elementType
              if (elementType == null) itemType = NodeKindTest.DOCUMENT
              else {
                val e = sequenceTypeFromTree(elementType, config).getPrimaryType
                itemType = new DocumentNodeTest(e.asInstanceOf[NodeTest])
              }
            case "NE" =>
              if (qName == null) if (contentTest == null) itemType = NodeKindTest.ELEMENT
              else itemType = contentTest
              else {
                itemType = new NameTest(Type.ELEMENT, qName.getURI, qName.getLocalPart, config.getNamePool)
                if (contentTest != null) itemType = new CombinedNodeTest(itemType.asInstanceOf[NodeTest], Token.INTERSECT, contentTest)
              }
            case "NA" =>
              if (qName == null) if (contentTest == null) itemType = NodeKindTest.ATTRIBUTE
              else itemType = contentTest
              else {
                itemType = new NameTest(Type.ATTRIBUTE, qName.getURI, qName.getLocalPart, config.getNamePool)
                if (contentTest != null) itemType = new CombinedNodeTest(itemType.asInstanceOf[NodeTest], Token.INTERSECT, contentTest)
              }
            case "NES" =>
              assert(qName != null)
              val decl: SchemaDeclaration = config.getElementDeclaration(qName)
              if (decl != null) try itemType = decl.makeSchemaNodeTest
              catch {
                case e: MissingComponentException =>


              }
              if (itemType == null) itemType = new NameTest(Type.ELEMENT, qName.getURI, qName.getLocalPart, config.getNamePool)
            case "NAS" =>
              assert(qName != null)
              val decl: SchemaDeclaration = config.getAttributeDeclaration(qName)
              if (decl != null) try itemType = decl.makeSchemaNodeTest
              catch {
                case e: MissingComponentException =>
              }
              if (itemType == null) itemType = new NameTest(Type.ATTRIBUTE, qName.getURI, qName.getLocalPart, config.getNamePool)
            case _ =>
              itemType = AnyNodeTest.getInstance
          }
        }
      }
    }
    else if (principal.startsWith("F")) if (principal == "FA") {
      val valueType = tree.valueType
      if (valueType == null) itemType = ArrayItemType.ANY_ARRAY_TYPE
      else itemType = new ArrayItemType(sequenceTypeFromTree(valueType, config))
    }
    else if (principal == "FM") if (tree.fieldNames == null) {
      val keyType = tree.keyType
      val valueType = tree.valueType
      if (keyType != null && valueType != null) {
        val a = sequenceTypeFromTree(keyType, config).getPrimaryType.asInstanceOf[AtomicType]
        val v = sequenceTypeFromTree(valueType, config)
        itemType = new MapType(a, v)
      }
      else itemType = MapType.ANY_MAP_TYPE
    }
    else {
      val fieldTypes = new util.ArrayList[SequenceType](tree.argTypes.size)

      for (t <- tree.argTypes.asScala) {
        fieldTypes.add(sequenceTypeFromTree(t, config))
      }
      itemType = new TupleItemType(tree.fieldNames, fieldTypes, tree.extensibleTupleType)
    }
    else {
      val returnType = tree.resultType
      val argTypes = tree.argTypes
      if (argTypes == null) itemType = AnyFunctionType
      else {
        var r: SequenceType = null
        if (returnType == null) r = SequenceType.ANY_SEQUENCE
        else r = sequenceTypeFromTree(returnType, config)
        val a = new Array[SequenceType](argTypes.size)
        for (i <- 0 until a.length) {
          a(i) = sequenceTypeFromTree(argTypes.get(i), config)
        }
        itemType = new SpecificFunctionType(a, r)
      }
    }
    val indicator = tree.cardinality
    val cardinality: Int = Cardinality.fromOccurrenceIndicator(indicator).asInstanceOf[Int]
    SequenceType.makeSequenceType(itemType, cardinality)
  }

  private def makeTree(sequenceType: SequenceType): AlphaCodeTree = {
    val tree = makeTree(sequenceType.getPrimaryType)
    if (sequenceType.getCardinality != StaticProperty.EXACTLY_ONE) tree.cardinality = Cardinality.getOccurrenceIndicator(sequenceType.getCardinality)
    tree
  }

  private def makeTree(primary: ItemType): AlphaCodeTree = {
    val result = new AlphaCode.AlphaCodeTree
    result.principal = primary.getBasicAlphaCode
    result.cardinality = "1"
    if (primary.isInstanceOf[AtomicType] && !(primary.asInstanceOf[AtomicType]).isBuiltInType) result.name = primary.asInstanceOf[AtomicType].getEQName
    else if (primary.isInstanceOf[UnionType]) {
      val name = primary.asInstanceOf[UnionType].getTypeName
      if (name.getURI == NamespaceConstant.SCHEMA) {
        result.name = "~" + name.getLocalPart
      }
      else if (name.getURI == NamespaceConstant.ANONYMOUS) {
        try {
          val memberMaps = new util.ArrayList[AlphaCode.AlphaCodeTree]

          for (pt <- primary.asInstanceOf[UnionType].getPlainMemberTypes) {
            memberMaps.add(makeTree(pt))
          }
          result.members = memberMaps
        } catch {
          case e: MissingComponentException =>
        }
      }
      else result.name = name.getEQName
    }
    else if (primary.isInstanceOf[NameTest]) {
      val name = primary.asInstanceOf[NameTest].getMatchingNodeName
      result.name = name.getEQName
    }
    else if (primary.isInstanceOf[SchemaNodeTest]) {
      val name = primary.asInstanceOf[SchemaNodeTest].getNodeName
      result.name = name.getEQName
    }
    else if (primary.isInstanceOf[LocalNameTest]) result.name = "*:" + primary.asInstanceOf[LocalNameTest].getLocalName
    else if (primary.isInstanceOf[NamespaceTest]) result.name = "Q{" + primary.asInstanceOf[NamespaceTest].getNamespaceURI + "}*"
    else if (primary.isInstanceOf[CombinedNodeTest]) {
      val combi = primary.asInstanceOf[CombinedNodeTest]
      val c = combi.getContentTypeForAlphaCode
      if (c != null) {
        result.content = c
        result.name = combi.getMatchingNodeName.getEQName
        result.nillable = combi.isNillable
      }
      else {
        result.vennOperator = combi.getOperator
        result.vennOperands = new Array[AlphaCode.AlphaCodeTree](2)
        result.vennOperands(0) = makeTree(combi.getOperand(0))
        result.vennOperands(1) = makeTree(combi.getOperand(1))
      }
    }
    else if (primary.isInstanceOf[MultipleNodeKindTest]) {
      result.vennOperator = Token.UNION
      val types = primary.getUType.decompose
      result.vennOperands = new Array[AlphaCode.AlphaCodeTree](types.size)
      var i = 0

      for (itemType <- types.asScala) {
        result.vennOperands({
          i += 1;
          i - 1
        }) = makeTree(itemType.toItemType)
      }
    }
    else if (primary.isInstanceOf[ContentTypeTest]) result.content = primary.asInstanceOf[ContentTypeTest].getContentType.getEQName
    else if (primary.isInstanceOf[DocumentNodeTest]) {
      val content = primary.asInstanceOf[DocumentNodeTest].getElementTest
      result.elementType = makeTree(content)
    }
    else if (primary.isInstanceOf[FunctionItemType]) if (primary.isInstanceOf[ArrayItemType]) {
      val memberType = primary.asInstanceOf[ArrayItemType].getMemberType
      if (memberType ne SequenceType.ANY_SEQUENCE) result.valueType = makeTree(memberType)
    }
    else if (primary.isInstanceOf[TupleItemType]) {
      result.extensibleTupleType = primary.asInstanceOf[TupleItemType].isExtensible
      result.fieldNames = new util.ArrayList[String]
      result.argTypes = new util.ArrayList[AlphaCode.AlphaCodeTree]

      for (s <- primary.asInstanceOf[TupleItemType].getFieldNames.asScala) {
        result.fieldNames.add(s)
        result.argTypes.add(makeTree(primary.asInstanceOf[TupleItemType].getFieldType(s)))
      }
    }
    else if (primary.isInstanceOf[MapType]) {
      val keyType = primary.asInstanceOf[MapType].getKeyType
      if (keyType ne BuiltInAtomicType.ANY_ATOMIC) result.keyType = makeTree(keyType)
      val valueType = primary.asInstanceOf[MapType].getValueType
      if (valueType ne SequenceType.ANY_SEQUENCE) result.valueType = makeTree(valueType)
    }
    else {
      val resultType = primary.asInstanceOf[FunctionItemType].getResultType
      if (resultType ne SequenceType.ANY_SEQUENCE) result.resultType = makeTree(resultType)
      val argTypes = primary.asInstanceOf[FunctionItemType].getArgumentTypes
      if (argTypes != null) {
        val argMaps = new util.ArrayList[AlphaCode.AlphaCodeTree]
        for (at <- argTypes) {
          argMaps.add(makeTree(at))
        }
        result.argTypes = argMaps
      }
    }
    result
  }

  private def abbreviateEQName(in: String) = if (in.startsWith("Q{" + NamespaceConstant.SCHEMA + "}")) "~" + in.substring(("Q{" + NamespaceConstant.SCHEMA + "}").length)
  else in

  private def alphaCodeFromTree(tree: AlphaCode.AlphaCodeTree, withCardinality: Boolean, sb: StringBuilder): Unit = {
    if (withCardinality) sb.append(tree.cardinality)
    sb.append(tree.principal)
    if (tree.name != null) sb.append(" n").append(abbreviateEQName(tree.name))
    if (tree.content != null) {
      sb.append(" c").append(abbreviateEQName(tree.content))
      if (tree.nillable) sb.append("?")
    }
    if (tree.keyType != null) {
      sb.append(" k[")
      alphaCodeFromTree(tree.keyType, withCardinality = false, sb)
      sb.append("]")
    }
    if (tree.valueType != null) {
      sb.append(" v[")
      alphaCodeFromTree(tree.valueType, withCardinality = true, sb)
      sb.append("]")
    }
    if (tree.resultType != null) {
      sb.append(" r[")
      alphaCodeFromTree(tree.resultType, withCardinality = true, sb)
      sb.append("]")
    }
    if (tree.argTypes != null) {
      sb.append(" a[")
      var first = true

      for (a <- tree.argTypes.asScala) {
        if (first) first = false
        else sb.append(",")
        alphaCodeFromTree(a, withCardinality = true, sb)
      }
      sb.append("]")
    }
    if (tree.members != null) {
      sb.append(" m[")
      var first = true

      for (a <- tree.members.asScala) {
        if (first) first = false
        else sb.append(",")
        alphaCodeFromTree(a, withCardinality = false, sb)
      }
      sb.append("]")
    }
    if (tree.elementType != null) {
      sb.append(" e[")
      alphaCodeFromTree(tree.elementType, withCardinality = false, sb)
      sb.append("]")
    }
    if (tree.vennOperands != null) {
      val operator = if (tree.vennOperator == Token.INTERSECT) "i"
      else if (tree.vennOperator == Token.UNION) "u"
      else "d"
      sb.append(" ").append(operator).append("[")
      for (i <- 0 until tree.vennOperands.length) {
        if (i != 0) sb.append(",")
        alphaCodeFromTree(tree.vennOperands(i), withCardinality = false, sb)
      }
      sb.append("]")
    }
    if (tree.fieldNames != null) {
      sb.append(if (tree.extensibleTupleType) " F["
      else " f[")
      var first = true

      for (s <- tree.fieldNames.asScala) {
        if (!first) sb.append(",")
        else first = false
        sb.append(s.replace("\\", "\\\\").replace(",", "\\,").replace("]", "\\]"))
      }
      sb.append("]")
    }
  }

  def fromItemType(`type`: ItemType) = {
    val tree = makeTree(`type`)
    val sb = new StringBuilder
    alphaCodeFromTree(tree, withCardinality = false, sb)
    sb.toString.trim
  }

  def fromSequenceType(`type`: SequenceType): String = {
    if (`type` eq SequenceType.EMPTY_SEQUENCE) return "0"
    val s = fromItemType(`type`.getPrimaryType)
    if (`type`.getCardinality == StaticProperty.EXACTLY_ONE) "1" + s
    else Cardinality.getOccurrenceIndicator(`type`.getCardinality) + s
  }

  @throws[XPathException]
  def fromLexicalSequenceType(context: XPathContext, input: String) = {
    val parser = context.getConfiguration.newExpressionParser("XP", updating = false, 31)
    val env = new IndependentContext(context.getConfiguration)
    env.declareNamespace("xs", NamespaceConstant.SCHEMA)
    env.declareNamespace("fn", NamespaceConstant.FN)
    val st = parser.parseSequenceType(input, env)
    fromSequenceType(st)
  }
}

class AlphaCode {}