package net.sf.saxon.trans

import net.sf.saxon.utils.Configuration
import net.sf.saxon.event._
import net.sf.saxon.expr._
import net.sf.saxon.expr.accum.Accumulator
import net.sf.saxon.expr.accum.AccumulatorRule
import net.sf.saxon.expr.compat.ArithmeticExpression10
import net.sf.saxon.expr.compat.GeneralComparison10
import net.sf.saxon.expr.flwor.LocalVariableBinding
import net.sf.saxon.expr.instruct._
import net.sf.saxon.expr.parser._
import net.sf.saxon.expr.sort._
import net.sf.saxon.functions._
import net.sf.saxon.functions.hof._
import net.sf.saxon.functions.registry.ConstructorFunctionLibrary
import net.sf.saxon.lib._
import net.sf.saxon.ma.arrays.ArrayFunctionSet
import net.sf.saxon.ma.arrays.SimpleArrayItem
import net.sf.saxon.ma.arrays.SquareArrayConstructor
import net.sf.saxon.ma.json.JsonParser
import net.sf.saxon.ma.map.HashTrieMap
import net.sf.saxon.ma.map.MapFunctionSet
import net.sf.saxon.model._
import net.sf.saxon.om._
import net.sf.saxon.pattern._
import net.sf.saxon.query.XQueryFunctionLibrary
import net.sf.saxon.s9api.HostLanguage
import net.sf.saxon.s9api.Location
import net.sf.saxon.serialize.CharacterMap
import scala.util.control.Breaks._

import scala.jdk.CollectionConverters._

/*import net.sf.saxon.style.PackageVersion
import net.sf.saxon.style.StylesheetFunctionLibrary*/
import net.sf.saxon.style.StylesheetPackage
import net.sf.saxon.sxpath.IndependentContext
import net.sf.saxon.trans.packages.IPackageLoader
import net.sf.saxon.trans.rules.BuiltInRuleSet
import net.sf.saxon.trans.rules.Rule

/*import net.sf.saxon.trans.rules.RuleManager*/
import net.sf.saxon.tree.iter.AxisIterator
import net.sf.saxon.tree.util.Navigator
import net.sf.saxon.tree.util.Orphan
import net.sf.saxon.tree.wrapper.VirtualCopy
import net.sf.saxon.value._
import net.sf.saxon.z.IntHashMap
import javax.xml.transform.Source
import javax.xml.transform.stream.StreamSource
import java.io.IOException
import java.io.LineNumberReader
import java.io.StringReader
import java.math.BigInteger
import java.util._

/**
 * This class reads the XML exported form of a package and reconstructs the package object in memory.
 */
object PackageLoaderHE {
  private val SAXON9911 = new NestedIntegerValue(Array[Int](9, 9, 1, 1))

  def processAccumulatorList(loader: PackageLoaderHE, inst: SourceDocument, accumulatorNames: String) = if (accumulatorNames != null) {
    val accNameList = new ArrayList[StructuredQName]
    val tokenizer = new StringTokenizer(accumulatorNames)
    while ( {
      tokenizer.hasMoreTokens
    }) {
      val token = tokenizer.nextToken
      val name = StructuredQName.fromEQName(token)
      accNameList.add(name)
    }
    val pack = loader.getPackStack.peek
    loader.addCompletionAction(() => {
      def foo() = {
        val list = new HashSet[Accumulator]

        for (sn <- accNameList.asScala) {

          for (test <- pack.getAccumulatorRegistry.getAllAccumulators.asScala) {
            if (test.getAccumulatorName == sn) list.add(test)
          }
        }
        inst.setUsedAccumulators(list)
      }

      foo()
    })
  }

  trait ExpressionLoader {
    @throws[XPathException]
    def loadFrom(loader: PackageLoaderHE, element: NodeInfo): Expression
  }

  trait PatternLoader {
    @throws[XPathException]
    def loadFrom(loader: PackageLoaderHE, element: NodeInfo): Pattern
  }

  val eMap = new HashMap[String, PackageLoaderHE.ExpressionLoader](200)
  val licensableConstructs = new HashMap[String, String](30)

  private def getLevelCode(levelAtt: String) = {
    var level = 0
    if (levelAtt == null) level = NumberInstruction.SINGLE
    else if (levelAtt == "single") level = NumberInstruction.SINGLE
    else if (levelAtt == "multi") level = NumberInstruction.MULTI
    else if (levelAtt == "any") level = NumberInstruction.ANY
    else if (levelAtt == "simple") level = NumberInstruction.SIMPLE
    else throw new AssertionError
    level
  }

  @throws[XPathException]
  def getChildExpressionList(loader: PackageLoaderHE, element: NodeInfo) = {
    val children = new ArrayList[Expression]
    val iter = element.iterateAxis(AxisInfo.CHILD, NodeKindTest.ELEMENT)
    var child: NodeInfo = null
    while ( {
      (child = iter.next) != null
    }) children.add(loader.loadExpression(child))
    children
  }

  @throws[XPathException]
  def getChildExpressionArray(loader: PackageLoaderHE, element: NodeInfo) = {
    val children = getChildExpressionList(loader, element)
    children.toArray(new Array[Expression](0))
  }

  def getOperator(opAtt: String) = {
    var op = 0
    opAtt match {
      case "=" =>
        op = Token.EQUALS
      case "!=" =>
        op = Token.NE
      case "<=" =>
        op = Token.LE
      case ">=" =>
        op = Token.GE
      case "<" =>
        op = Token.LT
      case ">" =>
        op = Token.GT
      case _ =>
        throw new IllegalStateException
    }
    op
  }

  private def parseValueComparisonOperator(opAtt: String) = {
    var op = 0
    opAtt match {
      case "eq" =>
        op = Token.FEQ
      case "ne" =>
        op = Token.FNE
      case "le" =>
        op = Token.FLE
      case "ge" =>
        op = Token.FGE
      case "lt" =>
        op = Token.FLT
      case "gt" =>
        op = Token.FGT
      case _ =>
        throw new IllegalStateException
    }
    op
  }

  val pMap = new HashMap[String, PackageLoaderHE.PatternLoader](200)
  try licensableConstructs.put("gcEE", "EE")
  licensableConstructs.put("indexedFilter", "EE")
  licensableConstructs.put("indexedFilter2", "EE")
  licensableConstructs.put("indexedLookup", "EE")
  licensableConstructs.put("stream", "EE")
  licensableConstructs.put("switch", "EE")
  licensableConstructs.put("acFnRef", "PE")
  licensableConstructs.put("assign", "PE")
  licensableConstructs.put("do", "PE")
  licensableConstructs.put("javaCall", "PE")
  licensableConstructs.put("while", "PE")
  eMap.put("among", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val lhs = loader.getFirstChildExpression(element)
      val rhs = loader.getSecondChildExpression(element)
      new SingletonIntersectExpression(lhs, Token.INTERSECT, rhs)
    }

    foo(loader, element)
  })
  eMap.put("analyzeString", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val select = loader.getExpressionWithRole(element, "select")
      val regex = loader.getExpressionWithRole(element, "regex")
      val flags = loader.getExpressionWithRole(element, "flags")
      val matching = loader.getExpressionWithRole(element, "matching")
      val nonMatching = loader.getExpressionWithRole(element, "nonMatching")
      val instr = new AnalyzeString(select, regex, flags, matching, nonMatching, null)
      instr.precomputeRegex(loader.getConfiguration, null)
      instr
    }

    foo(loader, element)
  })
  eMap.put("and", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val lhs = loader.getFirstChildExpression(element)
      val rhs = loader.getSecondChildExpression(element)
      new AndExpression(lhs, rhs)
    }

    foo(loader, element)
  })
  eMap.put("applyImports", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo): ApplyImports = {
      val inst = new ApplyImports
      val actuals = loader.loadWithParams(element, inst, false)
      val tunnels = loader.loadWithParams(element, inst, true)
      inst.setActualParams(actuals)
      inst.setTunnelParams(tunnels)
      inst
    }

    foo(loader, element)
  })
  eMap.put("applyT", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val pack = loader.packStack.peek
      val select = loader.getFirstChildExpression(element)
      val modeAtt = loader.getQNameAttribute(element, "mode")
      var mode = null
      /* if (modeAtt != null) mode = pack.getRuleManager.obtainMode(modeAtt, true).asInstanceOf[SimpleMode]
       else mode = pack.getRuleManager.obtainMode(null, true).asInstanceOf[SimpleMode]*/
      var flags = element.getAttributeValue("", "flags")
      if (flags == null) flags = ""
      val useCurrentMode = flags.contains("c")
      val useTailRecursion = flags.contains("t")
      val implicitSelect = flags.contains("i")
      val inStreamableConstruct = flags.contains("d")
      val inst = new ApplyTemplates(select, useCurrentMode, useTailRecursion, implicitSelect, inStreamableConstruct, mode /*, loader.packStack.peek.getRuleManager*/)
      val actuals = loader.loadWithParams(element, inst, false)
      val tunnels = loader.loadWithParams(element, inst, true)
      inst.setActualParams(actuals)
      inst.setTunnelParams(tunnels)
      val bindingSlot = loader.getIntegerAttribute(element, "bSlot")
      inst.setBindingSlot(bindingSlot)
      inst
    }

    foo(loader, element)
  })
  eMap.put("arith", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val lhs = loader.getFirstChildExpression(element)
      val rhs = loader.getSecondChildExpression(element)
      val code = element.getAttributeValue("", "calc")
      val calc = Calculator.reconstructCalculator(code)
      val operator = Calculator.operatorFromCode(code.charAt(1))
      val token = Calculator.getTokenFromOperator(operator)
      val exp = new ArithmeticExpression(lhs, token, rhs)
      exp.setCalculator(calc)
      exp
    }

    foo(loader, element)
  })
  eMap.put("arith10", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo): ArithmeticExpression = {
      val lhs = loader.getFirstChildExpression(element)
      val rhs = loader.getSecondChildExpression(element)
      val code = element.getAttributeValue("", "calc")
      val calc = Calculator.reconstructCalculator(code)
      val operator = Calculator.operatorFromCode(code.charAt(1))
      val token = Calculator.getTokenFromOperator(operator)
      val exp = new ArithmeticExpression10(lhs, token, rhs)
      exp.setCalculator(calc)
      exp
    }

    foo(loader, element)
  })
  eMap.put("array", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val children = getChildExpressionList(loader, element)
      val values = new ArrayList[GroundedValue](children.size)

      for (child <- children.asScala) {
        values.add(child.asInstanceOf[Literal].getValue)
      }
      Literal.makeLiteral(new SimpleArrayItem(values))
    }

    foo(loader, element)
  })
  eMap.put("arrayBlock", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo): Expression = {
      val children = getChildExpressionList(loader, element)
      new SquareArrayConstructor(children)
    }

    foo(loader, element)
  })
  eMap.put("atomic", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo): Literal = {
      val valAtt = element.getAttributeValue("", "val")
      val `type` = loader.parseAlphaCodeForItemType(element, "type").asInstanceOf[AtomicType]
      val `val` = `type`.getStringConverter(loader.config.getConversionRules).convertString(valAtt).asAtomic
      Literal.makeLiteral(`val`)
    }

    foo(loader, element)
  })
  eMap.put("atomSing", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo): Expression = {
      val body = loader.getFirstChildExpression(element)
      val role = RoleDiagnostic.reconstruct(element.getAttributeValue("", "diag"))
      val cardAtt = element.getAttributeValue("", "card")
      val allowEmpty = "?" == cardAtt
      new SingletonAtomizer(body, role, allowEmpty)
    }

    foo(loader, element)
  })
  eMap.put("att", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo): FixedAttribute = {
      val displayName = element.getAttributeValue("", "name")
      var parts: Array[String] = null
      try parts = NameChecker.getQNameParts(displayName)
      catch {
        case err: QNameException =>
          throw new XPathException(err)
      }
      var uri = element.getAttributeValue("", "nsuri")
      if (uri == null) uri = ""
      val name = new StructuredQName(parts(0), uri, parts(1))
      val attName = new FingerprintedQName(name, loader.config.getNamePool)
      var validation = Validation.SKIP
      val valAtt = element.getAttributeValue("", "validation")
      if (valAtt != null) validation = Validation.getCode(valAtt)
      val schemaType = loader.getTypeAttribute(element, "type")
      if (schemaType != null) validation = Validation.BY_TYPE
      val content = loader.getFirstChildExpression(element)
      val att = new FixedAttribute(attName, validation, schemaType.asInstanceOf[SimpleType])
      att.setSelect(content)
      att
    }

    foo(loader, element)
  })
  eMap.put("attVal", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo): AttributeGetter = {
      val name = loader.getQNameAttribute(element, "name")
      val attName = new FingerprintedQName(name, loader.config.getNamePool)
      val getter = new AttributeGetter(attName)
      getter.setRequiredChecks(loader.getIntegerAttribute(element, "chk"))
      getter
    }

    foo(loader, element)
  })
  eMap.put("axis", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo): Expression = {
      val axisName = element.getAttributeValue("", "name")
      val axis = AxisInfo.getAxisNumber(axisName)
      val nt = loader.parseAlphaCodeForItemType(element, "nodeTest").asInstanceOf[NodeTest]
      new AxisExpression(axis, nt)
    }

    foo(loader, element)
  })
  eMap.put("break", (loader: PackageLoaderHE, element: NodeInfo) => new BreakInstr)
  eMap.put("callT", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val pack = loader.packStack.peek
      val name = loader.getQNameAttribute(element, "name")
      val symbol = new SymbolicName(StandardNames.XSL_TEMPLATE, name)
      val target = pack.getComponent(symbol)
      var t: NamedTemplate = null
      if (target == null) t = new NamedTemplate(name)
      else t = target.getActor.asInstanceOf[NamedTemplate]
      val flags = element.getAttributeValue("", "flags")
      val useTailRecursion = flags != null && flags.contains("t")
      val inStreamableConstruct = flags != null && flags.contains("d")
      val inst = new CallTemplate(t, name, useTailRecursion, inStreamableConstruct)
      val actuals = loader.loadWithParams(element, inst, false)
      val tunnels = loader.loadWithParams(element, inst, true)
      inst.setActualParameters(actuals, tunnels)
      val bindingSlot = loader.getIntegerAttribute(element, "bSlot")
      inst.setBindingSlot(bindingSlot)
      loader.fixups.peek.add(inst)
      inst
    }

    foo(loader, element)
  })
  eMap.put("cast", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val body = loader.getFirstChildExpression(element)
      val flags = element.getAttributeValue("", "flags")
      val allowEmpty = flags.contains("e")
      if (flags.contains("a")) {
        val seqType = loader.parseAlphaCode(element, "as")
        new CastExpression(body, seqType.getPrimaryType.asInstanceOf[AtomicType], allowEmpty)
      }
      else if (flags.contains("l")) {
        val typeName = StructuredQName.fromEQName(element.getAttributeValue("", "as"))
        val `type` = loader.config.getSchemaType(typeName)
        val resolver = element.getAllNamespaces
        val ucf = new ListConstructorFunction(`type`.asInstanceOf[ListType], resolver, allowEmpty)
        new StaticFunctionCall(ucf, Array[Expression](body))
      }
      else if (flags.contains("u")) if (element.getAttributeValue("", "as") != null) {
        val typeName = StructuredQName.fromEQName(element.getAttributeValue("", "as"))
        val `type` = loader.config.getSchemaType(typeName)
        val resolver = element.getAllNamespaces
        val ucf = new UnionConstructorFunction(`type`.asInstanceOf[UnionType], resolver, allowEmpty)
        new StaticFunctionCall(ucf, Array[Expression](body))
      }
      else {
        val `type` = loader.parseAlphaCode(element, "to").getPrimaryType.asInstanceOf[LocalUnionType]
        val resolver = element.getAllNamespaces
        val ucf = new UnionConstructorFunction(`type`, resolver, allowEmpty)
        new StaticFunctionCall(ucf, Array[Expression](body))
      }
      else throw new AssertionError("Unknown simple type variety " + flags)
    }

    foo(loader, element)
  })
  eMap.put("castable", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo): Expression = {
      val body = loader.getFirstChildExpression(element)
      val flags = element.getAttributeValue("", "flags")
      val allowEmpty = flags.contains("e")
      if (flags.contains("a")) {
        val seqType = loader.parseAlphaCode(element, "as")
        new CastableExpression(body, seqType.getPrimaryType.asInstanceOf[AtomicType], allowEmpty)
      }
      else if (flags.contains("l")) {
        val typeName = StructuredQName.fromEQName(element.getAttributeValue("", "as"))
        val `type` = loader.config.getSchemaType(typeName)
        val resolver = element.getAllNamespaces
        val ucf = new ListCastableFunction(`type`.asInstanceOf[ListType], resolver, allowEmpty)
        new StaticFunctionCall(ucf, Array[Expression](body))
      }
      else if (flags.contains("u")) if (element.getAttributeValue("", "as") != null) {
        val typeName = StructuredQName.fromEQName(element.getAttributeValue("", "as"))
        val `type` = loader.config.getSchemaType(typeName)
        val resolver = element.getAllNamespaces
        val ucf = new UnionCastableFunction(`type`.asInstanceOf[UnionType], resolver, allowEmpty)
        new StaticFunctionCall(ucf, Array[Expression](body))
      }
      else {
        val `type` = loader.parseAlphaCode(element, "to").getPrimaryType.asInstanceOf[LocalUnionType]
        val resolver = element.getAllNamespaces
        val ucf = new UnionCastableFunction(`type`, resolver, allowEmpty)
        new StaticFunctionCall(ucf, Array[Expression](body))
      }
      else throw new AssertionError("Unknown simple type variety " + flags)
      //            Expression body = loader.getFirstChildExpression(element);
      //            SchemaType st = loader.getTypeAttribute(element, "as");
      //            boolean allowEmpty = element.getAttributeValue("", "emptiable").equals("1");
      //            if (st == null) {
      //                throw new AssertionError("Unknown simple type " + element.getAttributeValue("", "as"));
      //            } else if (st instanceof AtomicType) {
      //                return new CastableExpression(body, (AtomicType) st, allowEmpty);
      //            } else if (st instanceof ListType) {
      //                NamespaceResolver resolver = element.getAllNamespaces();
      //                ListCastableFunction ucf = new ListCastableFunction((ListType) st, resolver, allowEmpty);
      //                return new StaticFunctionCall(ucf, new Expression[]{body});
      //            } else if (st instanceof UnionType) {
      //                UnionCastableFunction ucf = new UnionCastableFunction((UnionType) st, resolver, allowEmpty);
      //            } else {
      //                throw new AssertionError("Unknown simple type variety " + st.getClass());
      //            }
    }

    foo(loader, element)
  })
  eMap.put("check", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo): Expression = {
      val body = loader.getFirstChildExpression(element)
      val cardAtt = element.getAttributeValue("", "card")
      var c = 0
      cardAtt match {
        case "?" =>
          c = StaticProperty.ALLOWS_ZERO_OR_ONE
        case "*" =>
          c = StaticProperty.ALLOWS_ZERO_OR_MORE
        case "+" =>
          c = StaticProperty.ALLOWS_ONE_OR_MORE
        case "\u00B0" => // Obsolescent, drop this
        case "0" =>
          c = StaticProperty.ALLOWS_ZERO
        case "1" =>
          c = StaticProperty.EXACTLY_ONE
        case _ =>
          throw new IllegalStateException("Occurrence indicator: '" + cardAtt + "'")
      }
      val role = RoleDiagnostic.reconstruct(element.getAttributeValue("", "diag"))
      CardinalityChecker.makeCardinalityChecker(body, c, role)
    }

    foo(loader, element)
  })
  eMap.put("choose", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo): Choose = {
      val conditions = new ArrayList[Expression]
      val actions = new ArrayList[Expression]
      val iter = element.iterateAxis(AxisInfo.CHILD, NodeKindTest.ELEMENT)
      var child: NodeInfo = null
      var odd = true
      while ( {
        (child = iter.next) != null
      }) {
        if (odd) conditions.add(loader.loadExpression(child))
        else actions.add(loader.loadExpression(child))
        odd = !odd
      }
      new Choose(conditions.toArray(new Array[Expression](0)), actions.toArray(new Array[Expression](0)))
    }

    foo(loader, element)
  })
  eMap.put("comment", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val select = loader.getFirstChildExpression(element)
      val inst = new Comment
      inst.setSelect(select)
      inst
    }

    foo(loader, element)
  })
  eMap.put("compareToInt", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val i = new BigInteger(element.getAttributeValue("", "val"))
      val opAtt = element.getAttributeValue("", "op")
      val lhs = loader.getFirstChildExpression(element)
      new CompareToIntegerConstant(lhs, parseValueComparisonOperator(opAtt), i.longValue)
    }

    foo(loader, element)
  })
  eMap.put("compareToString", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val s = element.getAttributeValue("", "val")
      val opAtt = element.getAttributeValue("", "op")
      val lhs = loader.getFirstChildExpression(element)
      new CompareToStringConstant(lhs, parseValueComparisonOperator(opAtt), s)
    }

    foo(loader, element)
  })
  eMap.put("compAtt", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val name = loader.getExpressionWithRole(element, "name")
      val namespace = loader.getExpressionWithRole(element, "namespace")
      val content = loader.getExpressionWithRole(element, "select")
      var validation = Validation.SKIP
      val valAtt = element.getAttributeValue("", "validation")
      if (valAtt != null) validation = Validation.getCode(valAtt)
      val schemaType = loader.getTypeAttribute(element, "type")
      if (schemaType != null) validation = Validation.BY_TYPE
      val att = new ComputedAttribute(name, namespace, null, validation, schemaType.asInstanceOf[SimpleType], false)
      att.setSelect(content)
      att
    }

    foo(loader, element)
  })
  eMap.put("compElem", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val name = loader.getExpressionWithRole(element, "name")
      val namespace = loader.getExpressionWithRole(element, "namespace")
      val content = loader.getExpressionWithRole(element, "content")
      var validation = Validation.SKIP
      val valAtt = element.getAttributeValue("", "validation")
      if (valAtt != null) validation = Validation.getCode(valAtt)
      val schemaType = loader.getTypeAttribute(element, "type")
      if (schemaType != null) validation = Validation.BY_TYPE
      val flags = element.getAttributeValue("", "flags")
      val inst = new ComputedElement(name, namespace, schemaType, validation, true, false)
      if (flags != null) inst.setInheritanceFlags(flags)
      inst.setContentExpression(content)
      inst.simplify
    }

    foo(loader, element)
  })
  // generated (redundantly) prior to 9.7.0.4
  eMap.put("compiledExpression", (loader: PackageLoaderHE, element: NodeInfo) => {
    loader.getFirstChildExpression(element)
  })
  eMap.put("conditionalSort", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val lhs = loader.getFirstChildExpression(element)
      val rhs = loader.getSecondChildExpression(element)
      new ConditionalSorter(lhs, rhs.asInstanceOf[DocumentSorter])
    }

    foo(loader, element)
  })
  eMap.put("condCont", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val base = loader.getFirstChildExpression(element)
      new WherePopulated(base)
    }

    foo(loader, element)
  })
  eMap.put("condSeq", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val args = getChildExpressionArray(loader, element)
      new ConditionalBlock(args)
    }

    foo(loader, element)
  })
  eMap.put("consume", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val arg = loader.getFirstChildExpression(element)
      new ConsumingOperand(arg)
    }

    foo(loader, element)
  })
  eMap.put("convert", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val body = loader.getFirstChildExpression(element)
      val fromType = loader.parseAlphaCodeForItemType(element, "from")
      val toType = loader.parseAlphaCodeForItemType(element, "to")
      val asc = new AtomicSequenceConverter(body, toType.asInstanceOf[PlainType])
      if ("p" == element.getAttributeValue("", "flags")) {
        val c = if (toType == BuiltInAtomicType.DOUBLE) new Converter.PromoterToDouble
        else new Converter.PromoterToFloat
        asc.setConverter(c)
      }
      else {
        val c = asc.allocateConverter(loader.config, false, fromType)
        asc.setConverter(c)
      }
      val diag = element.getAttributeValue("", "diag")
      if (diag != null) asc.setRoleDiagnostic(RoleDiagnostic.reconstruct(diag))
      asc
    }

    foo(loader, element)
  })
  eMap.put("copy", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      var validation = Validation.SKIP
      val valAtt = element.getAttributeValue("", "validation")
      if (valAtt != null) validation = Validation.getCode(valAtt)
      val schemaType = loader.getTypeAttribute(element, "type")
      if (schemaType != null) validation = Validation.BY_TYPE
      val sType = element.getAttributeValue("", "sit")
      val inst = new Copy(false, false, schemaType, validation)
      inst.setContentExpression(loader.getFirstChildExpression(element))
      val flags = element.getAttributeValue("", "flags")
      inst.setCopyNamespaces(flags.contains("c"))
      inst.setBequeathNamespacesToChildren(flags.contains("i"))
      inst.setInheritNamespacesFromParent(flags.contains("n"))
      if (sType != null) {
        val st = AlphaCode.toSequenceType(sType, loader.getConfiguration)
        inst.setSelectItemType(st.getPrimaryType)
      }
      inst
    }

    foo(loader, element)
  })
  eMap.put("copyOf", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val select = loader.getFirstChildExpression(element)
      var flags = element.getAttributeValue("", "flags")
      if (flags == null) flags = ""
      val copyNamespaces = flags.contains("c")
      val rejectDups = flags.contains("d")
      var validation = Validation.SKIP
      val valAtt = element.getAttributeValue("", "validation")
      if (valAtt != null) validation = Validation.getCode(valAtt)
      val schemaType = loader.getTypeAttribute(element, "type")
      if (schemaType != null) validation = Validation.BY_TYPE
      val inst = new CopyOf(select, copyNamespaces, validation, schemaType, rejectDups)
      inst.setCopyAccumulators(flags.contains("m"))
      inst.setCopyLineNumbers(flags.contains("l"))
      inst.setSchemaAware(flags.contains("s"))
      inst.setCopyForUpdate(flags.contains("u"))
      inst
    }

    foo(loader, element)
  })
  eMap.put("currentGroup", (loader: PackageLoaderHE, element: NodeInfo) => new CurrentGroupCall)
  eMap.put("currentGroupingKey", (loader: PackageLoaderHE, element: NodeInfo) => new CurrentGroupingKeyCall)
  eMap.put("curriedFunc", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val target = loader.getFirstChildExpression(element)
      val targetFn = target.asInstanceOf[Literal].getValue.asInstanceOf[Function]
      val args = loader.getChild(element, 1)
      var count = Count.count(args.iterateAxis(AxisInfo.CHILD, NodeKindTest.ELEMENT))
      val argValues = new Array[Sequence](count)
      count = 0

      for (child <- args.children(NodeKindTest.ELEMENT)) {
        if (child.getLocalPart == "x") argValues({
          count += 1;
          count - 1
        }) = null
        else {
          val arg = loader.loadExpression(child)
          argValues({
            count += 1;
            count - 1
          }) = arg.asInstanceOf[Literal].getValue
        }
      }
      val f = new CurriedFunction(targetFn, argValues)
      Literal.makeLiteral(f)
    }

    foo(loader, element)
  })
  eMap.put("cvUntyped", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val body = loader.getFirstChildExpression(element)
      val toType = loader.parseAlphaCodeForItemType(element, "to")
      if (toType.asInstanceOf[SimpleType].isNamespaceSensitive) UntypedSequenceConverter.makeUntypedSequenceRejector(loader.config, body, toType.asInstanceOf[PlainType])
      else {
        val cv = UntypedSequenceConverter.makeUntypedSequenceConverter(loader.config, body, toType.asInstanceOf[PlainType])
        val diag = element.getAttributeValue("", "diag")
        if (diag != null) cv.setRoleDiagnostic(RoleDiagnostic.reconstruct(diag))
        cv
      }
    }

    foo(loader, element)
  })
  eMap.put("data", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val body = loader.getFirstChildExpression(element)
      val diag = element.getAttributeValue("", "diag")
      new Atomizer(body, if (diag == null) null
      else RoleDiagnostic.reconstruct(diag))
    }

    foo(loader, element)
  })
  eMap.put("dbl", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val `val` = element.getAttributeValue("", "val")
      val d = StringToDouble.getInstance.stringToNumber(`val`)
      Literal.makeLiteral(new DoubleValue(d))
    }

    foo(loader, element)
  })
  eMap.put("dec", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val `val` = element.getAttributeValue("", "val")
      Literal.makeLiteral(BigDecimalValue.makeDecimalValue(`val`, false).asAtomic)
    }

    foo(loader, element)
  })
  eMap.put("doc", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      var validation = Validation.SKIP
      val valAtt = element.getAttributeValue("", "validation")
      if (valAtt != null) validation = Validation.getCode(valAtt)
      val schemaType = loader.getTypeAttribute(element, "type")
      if (schemaType != null) validation = Validation.BY_TYPE
      val flags = element.getAttributeValue("", "flags")
      val textOnly = flags != null && flags.contains("t")
      val base = element.getAttributeValue("", "base")
      val constantText = element.getAttributeValue("", "text")
      val body = loader.getFirstChildExpression(element)
      val inst = new DocumentInstr(textOnly, constantText)
      inst.setContentExpression(body)
      inst.setValidationAction(validation, schemaType)
      inst
    }

    foo(loader, element)
  })
  eMap.put("docOrder", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val select = loader.getFirstChildExpression(element)
      val intra = element.getAttributeValue("", "intra") == "1"
      new DocumentSorter(select, intra)
    }

    foo(loader, element)
  })
  eMap.put("dot", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val cie = new ContextItemExpression
      val st = loader.parseAlphaCode(element, "type")
      val `type` = st.getPrimaryType
      var maybeAbsent = false
      if ("a" == element.getAttributeValue("", "flags")) maybeAbsent = true
      val info = loader.getConfiguration.makeContextItemStaticInfo(`type`, maybeAbsent)
      cie.setStaticInfo(info)
      cie
    }

    foo(loader, element)
  })
  eMap.put("elem", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val displayName = element.getAttributeValue("", "name")
      var parts: Array[String] = null
      try parts = NameChecker.getQNameParts(displayName)
      catch {
        case err: QNameException =>
          throw new XPathException(err)
      }
      val nsuri = element.getAttributeValue("", "nsuri")
      val name = new StructuredQName(parts(0), nsuri, parts(1))
      val elemName = new FingerprintedQName(name, loader.config.getNamePool)
      val ns = element.getAttributeValue("", "namespaces")
      var bindings = NamespaceMap.emptyMap
      if (ns != null && !ns.isEmpty) {
        val pairs = ns.split(" ")
        val i = 0
        for (pair <- pairs) {
          val eq = pair.indexOf('=')
          if (eq >= 0) {
            var prefix = pair.substring(0, eq)
            if (prefix == "#") prefix = ""
            var uri = pair.substring(eq + 1)
            if (uri == "~") uri = NamespaceConstant.getUriForConventionalPrefix(prefix)
            bindings = bindings.put(prefix, uri)
          }
          else {
            val rsc = loader.contextStack.peek
            var prefix = pair
            if (prefix == "#") prefix = ""
            val uri = rsc.getURIForPrefix(prefix, true)
            assert(uri != null)
            bindings = bindings.put(prefix, uri)
          }
        }
      }
      var validation = Validation.SKIP
      val valAtt = element.getAttributeValue("", "validation")
      if (valAtt != null) validation = Validation.getCode(valAtt)
      val schemaType = loader.getTypeAttribute(element, "type")
      if (schemaType != null) validation = Validation.BY_TYPE
      val content = loader.getFirstChildExpression(element)
      val elem = new FixedElement(elemName, bindings, true, true, schemaType, validation)
      val flags = element.getAttributeValue("", "flags")
      if (flags != null) elem.setInheritanceFlags(flags)
      elem.setContentExpression(content)
      elem
    }

    foo(loader, element)
  })
  eMap.put("empty", (loader: PackageLoaderHE, element: NodeInfo) => Literal.makeLiteral(EmptySequence.getInstance))
  eMap.put("emptyTextNodeRemover", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val body = loader.getFirstChildExpression(element)
      new EmptyTextNodeRemover(body)
    }

    foo(loader, element)
  })
  eMap.put("error", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val message = element.getAttributeValue("", "message")
      val code = element.getAttributeValue("", "code")
      val isTypeErr = "1" == element.getAttributeValue("", "isTypeErr")
      new ErrorExpression(message, code, isTypeErr)
    }

    foo(loader, element)
  })
  eMap.put("evaluate", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val required = loader.parseAlphaCode(element, "as")
      val xpath = loader.getExpressionWithRole(element, "xpath")
      val contextItem = loader.getExpressionWithRole(element, "cxt")
      val baseUri = loader.getExpressionWithRole(element, "baseUri")
      val namespaceContext = loader.getExpressionWithRole(element, "nsCxt")
      val schemaAware = loader.getExpressionWithRole(element, "sa")
      val dynamicParams = loader.getExpressionWithRole(element, "wp")
      val optionsOp = loader.getExpressionWithRole(element, "options")
      val inst = new EvaluateInstr(xpath, required, contextItem, baseUri, namespaceContext, schemaAware)
      if (optionsOp != null) inst.setOptionsExpression(optionsOp)
      val namespaces = element.getAttributeValue("", "schNS")
      if (namespaces != null) {
        val uris = namespaces.split(" ")
        for (uri <- uris) {
          var uriStr = uri
          if (uriStr == "##") uriStr = ""
          inst.importSchemaNamespace(uriStr)
        }
      }
      val test = new NameTest(Type.ELEMENT, NamespaceConstant.SAXON_XSLT_EXPORT, "withParam", loader.getConfiguration.getNamePool)
      val nonTunnelParams = new ArrayList[WithParam]
      var slotNumber = 0

      for (wp <- element.children(test)) {
        val withParam = new WithParam
        val paramName = loader.getQNameAttribute(wp, "name")
        withParam.setVariableQName(paramName)
        withParam.setSlotNumber({
          slotNumber += 1;
          slotNumber - 1
        })
        val reqType = loader.parseAlphaCode(wp, "as")
        withParam.setRequiredType(reqType)
        withParam.setSelectExpression(inst, loader.getFirstChildExpression(wp))
        nonTunnelParams.add(withParam)
      }
      inst.setActualParameters(nonTunnelParams.toArray(new Array[WithParam](0)))
      if (dynamicParams != null) inst.setDynamicParams(dynamicParams)
      inst
    }

    foo(loader, element)
  })
  eMap.put("every", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val select = loader.getFirstChildExpression(element)
      val slot = loader.getIntegerAttribute(element, "slot")
      val name = loader.getQNameAttribute(element, "var")
      val requiredType = loader.parseAlphaCode(element, "as")
      val qEx = new QuantifiedExpression
      qEx.setOperator(Token.EVERY)
      qEx.setSequence(select)
      qEx.setRequiredType(requiredType)
      qEx.setSlotNumber(slot)
      qEx.setVariableQName(name)
      loader.localBindings.push(qEx)
      val action = loader.getSecondChildExpression(element)
      loader.localBindings.pop
      qEx.setAction(action)
      qEx
    }

    foo(loader, element)
  })
  eMap.put("except", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val lhs = loader.getFirstChildExpression(element)
      val rhs = loader.getSecondChildExpression(element)
      new VennExpression(lhs, Token.EXCEPT, rhs)
    }

    foo(loader, element)
  })
  eMap.put("false", (loader: PackageLoaderHE, element: NodeInfo) => Literal.makeLiteral(BooleanValue.FALSE))
  eMap.put("filter", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val lhs = loader.getFirstChildExpression(element)
      val rhs = loader.getSecondChildExpression(element)
      val flags = element.getAttributeValue("", "flags")
      val fe = new FilterExpression(lhs, rhs)
      fe.setFlags(flags)
      fe
    }

    foo(loader, element)
  })
  eMap.put("first", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val base = loader.getFirstChildExpression(element)
      FirstItemExpression.makeFirstItemExpression(base)
    }

    foo(loader, element)
  })
  eMap.put("fn", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val rsc = loader.makeRetainedStaticContext(element)
      loader.contextStack.push(rsc)
      val args = getChildExpressionArray(loader, element)
      var name = element.getAttributeValue("", "name")
      if (name == "_STRING-JOIN_2.0") { // encountered in files exported by Saxon 9.7
        name = "string-join"
      }
      val e = SystemFunction.makeCall(name, rsc, args: _*)
      if (e == null) throw new XPathException("Unknown system function " + name + "#" + args.length)
      if (e.isInstanceOf[SystemFunctionCall]) {
        val fn = e.asInstanceOf[SystemFunctionCall].getTargetFunction
        fn.setRetainedStaticContext(rsc)
        val iter = element.iterateAxis(AxisInfo.ATTRIBUTE)
        var att: NodeInfo = null
        val props = new Properties
        while ( {
          (att = iter.next) != null
        }) props.setProperty(att.getLocalPart, att.getStringValue)
        fn.importAttributes(props)
        loader.addCompletionAction(() => fn.fixArguments(args: _*))
      }
      loader.contextStack.pop
      e
    }

    foo(loader, element)
  })
  eMap.put("fnCoercer", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val `type` = loader.parseAlphaCode(element, "to").getPrimaryType.asInstanceOf[SpecificFunctionType]
      val role = RoleDiagnostic.reconstruct(element.getAttributeValue("", "diag"))
      val arg = loader.getFirstChildExpression(element)
      new FunctionSequenceCoercer(arg, `type`, role)
    }

    foo(loader, element)
  })
  eMap.put("fnRef", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      loader.needsPELicense("higher order functions")
      val name = element.getAttributeValue("", "name")
      val arity = loader.getIntegerAttribute(element, "arity")
      val rsc = loader.makeRetainedStaticContext(element)
      var f: SystemFunction = null
      if (name.startsWith("Q{")) {
        val qName = StructuredQName.fromEQName(name)
        val uri = qName.getURI
        uri match {
          case NamespaceConstant.MATH =>
            f = MathFunctionSet.getInstance.makeFunction(qName.getLocalPart, arity)
          case NamespaceConstant.MAP_FUNCTIONS =>
            f = MapFunctionSet.getInstance.makeFunction(qName.getLocalPart, arity)
          case NamespaceConstant.ARRAY_FUNCTIONS =>
            f = ArrayFunctionSet.getInstance.makeFunction(qName.getLocalPart, arity)
          case NamespaceConstant.SAXON =>
            f = loader.getConfiguration.bindSaxonExtensionFunction(qName.getLocalPart, arity)
        }
      }
      else f = SystemFunction.makeFunction(name, rsc, arity)
      if (f == null) throw new XPathException("Unknown system function " + name + "#" + arity, SaxonErrorCode.SXPK0002)
      new FunctionLiteral(f)
    }

    foo(loader, element)
  })
  eMap.put("follows", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val lhs = loader.getFirstChildExpression(element)
      val rhs = loader.getSecondChildExpression(element)
      new IdentityComparison(lhs, Token.FOLLOWS, rhs)
    }

    foo(loader, element)
  })
  eMap.put("for", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val select = loader.getFirstChildExpression(element)
      val slot = loader.getIntegerAttribute(element, "slot")
      val name = loader.getQNameAttribute(element, "var")
      val requiredType = loader.parseAlphaCode(element, "as")
      val forEx = new ForExpression
      forEx.setSequence(select)
      forEx.setRequiredType(requiredType)
      forEx.setSlotNumber(slot)
      forEx.setVariableQName(name)
      loader.localBindings.push(forEx)
      val action = loader.getSecondChildExpression(element)
      loader.localBindings.pop
      forEx.setAction(action)
      forEx
    }

    foo(loader, element)
  })
  eMap.put("forEach", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val lhs = loader.getFirstChildExpression(element)
      val rhs = loader.getSecondChildExpression(element)
      val threads = loader.getExpressionWithRole(element, "threads")
      if (threads == null) new ForEach(lhs, rhs)
      else {
        val forEach = new ForEach(lhs, rhs, false, threads)
        loader.getConfiguration.obtainOptimizer.generateMultithreadedInstruction(forEach)
      }
    }

    foo(loader, element)
  })
  eMap.put("forEachGroup", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val algorithmAtt = element.getAttributeValue("", "algorithm")
      var algo = 0
      if ("by" == algorithmAtt) algo = ForEachGroup.GROUP_BY
      else if ("adjacent" == algorithmAtt) algo = ForEachGroup.GROUP_ADJACENT
      else if ("starting" == algorithmAtt) algo = ForEachGroup.GROUP_STARTING
      else if ("ending" == algorithmAtt) algo = ForEachGroup.GROUP_ENDING
      else throw new AssertionError
      val flags = element.getAttributeValue("", "flags")
      val composite = flags != null && flags.contains("c")
      val inFork = flags != null && flags.contains("k")
      val select = loader.getExpressionWithRole(element, "select")
      var key: Expression = null
      if (algo == ForEachGroup.GROUP_BY || algo == ForEachGroup.GROUP_ADJACENT) key = loader.getExpressionWithRole(element, "key")
      else key = loader.getPatternWithRole(element, "match")
      var sortKeys = loader.loadSortKeyDefinitions(element)
      if (sortKeys.size == 0) sortKeys = null
      val collationNameExp = loader.getExpressionWithRole(element, "collation")
      val content = loader.getExpressionWithRole(element, "content")
      var collator: StringCollator = null
      if (collationNameExp.isInstanceOf[StringLiteral]) {
        val collationName = collationNameExp.asInstanceOf[StringLiteral].getStringValue
        collator = loader.config.getCollation(collationName)
      }
      val feg = new ForEachGroup(select, content, algo.toByte, key, collator, collationNameExp, sortKeys)
      feg.setComposite(composite)
      feg.setIsInFork(inFork)
      feg
    }

    foo(loader, element)
  })
  eMap.put("fork", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val args = getChildExpressionArray(loader, element)
      new Fork(args)
    }

    foo(loader, element)
  })
  eMap.put("gc", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val opAtt = element.getAttributeValue("", "op")
      val op = getOperator(opAtt)
      val lhs = loader.getFirstChildExpression(element)
      val rhs = loader.getSecondChildExpression(element)
      val compAtt = element.getAttributeValue("", "comp")
      val comp = loader.makeAtomicComparer(compAtt, element)
      val gc = new GeneralComparison20(lhs, op, rhs)
      gc.setAtomicComparer(comp)
      gc
    }

    foo(loader, element)
  })
  eMap.put("gc10", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val opAtt = element.getAttributeValue("", "op")
      val op = getOperator(opAtt)
      val lhs = loader.getFirstChildExpression(element)
      val rhs = loader.getSecondChildExpression(element)
      val compAtt = element.getAttributeValue("", "comp")
      val gc = new GeneralComparison10(lhs, op, rhs)
      val comp = loader.makeAtomicComparer(compAtt, element)
      gc.setAtomicComparer(comp)
      gc
    }

    foo(loader, element)
  })
  eMap.put("gVarRef", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val name = loader.getQNameAttribute(element, "name")
      val ref = new GlobalVariableReference(name)
      val bindingSlot = loader.getIntegerAttribute(element, "bSlot")
      ref.setBindingSlot(bindingSlot)
      loader.fixups.peek.add(ref)
      ref
    }

    foo(loader, element)
  })
  eMap.put("homCheck", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val body = loader.getFirstChildExpression(element)
      new HomogeneityChecker(body)
    }

    foo(loader, element)
  })
  eMap.put("ifCall", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val args = getChildExpressionArray(loader, element)
      val name = loader.getQNameAttribute(element, "name")
      var exp: Expression = null
      if (name.hasURI(NamespaceConstant.MATH)) exp = MathFunctionSet.getInstance.makeFunction(name.getLocalPart, args.length).makeFunctionCall(args: _*)
      else if (name.hasURI(NamespaceConstant.MAP_FUNCTIONS)) exp = MapFunctionSet.getInstance.makeFunction(name.getLocalPart, args.length).makeFunctionCall(args: _*)
      else if (name.hasURI(NamespaceConstant.ARRAY_FUNCTIONS)) exp = ArrayFunctionSet.getInstance.makeFunction(name.getLocalPart, args.length).makeFunctionCall(args: _*)
      else if (name.hasURI(NamespaceConstant.SAXON)) {
        loader.needsPELicense("Saxon extension functions")
        exp = null
      }
      if (exp == null) {
        val sName = new SymbolicName.F(name, args.length)
        val `type` = loader.parseAlphaCode(element, "type")
        val ic = new IndependentContext(loader.config)
        val rsc = loader.makeRetainedStaticContext(element)
        ic.setBaseURI(rsc.getStaticBaseUriString)
        ic.setPackageData(rsc.getPackageData)
        ic.setXPathLanguageLevel(31)
        ic.setDefaultElementNamespace(rsc.getDefaultElementNamespace)
        ic.setNamespaceResolver(rsc)
        ic.setBackwardsCompatibilityMode(rsc.isBackwardsCompatibility)
        ic.setDefaultCollationName(rsc.getDefaultCollationName)
        ic.setDefaultFunctionNamespace(rsc.getDefaultFunctionNamespace)
        ic.setDecimalFormatManager(rsc.getDecimalFormatManager)
        val reasons = new ArrayList[String]
        exp = loader.config.getIntegratedFunctionLibrary.bind(sName, args, ic, reasons)
        if (exp == null) exp = loader.config.getBuiltInExtensionLibraryList.bind(sName, args, ic, reasons)
        if (exp.isInstanceOf[SystemFunctionCall]) {
          val fn = exp.asInstanceOf[SystemFunctionCall].getTargetFunction
          fn.setRetainedStaticContext(loader.makeRetainedStaticContext(element))
          val iter = element.iterateAxis(AxisInfo.ATTRIBUTE)
          var att: NodeInfo = null
          val props = new Properties
          while ( {
            (att = iter.next) != null
          }) props.setProperty(att.getLocalPart, att.getStringValue)
          fn.importAttributes(props)
        }
        if (exp == null) {
          val msg = new StringBuilder("IntegratedFunctionCall to " + sName + " not found")

          for (reason <- reasons.asScala) {
            msg.append(". ").append(reason)
          }
          throw new XPathException(msg.toString)
        }
        if (exp.isInstanceOf[IntegratedFunctionCall]) {
          exp.asInstanceOf[IntegratedFunctionCall].getFunction.supplyStaticContext(ic, -1, args)
          exp.asInstanceOf[IntegratedFunctionCall].setResultType(`type`)
        }
      }
      exp
    }

    foo(loader, element)
  })
  eMap.put("inlineFn", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val first = loader.getChild(element, 0)
      val uf = loader.readFunction(first)
      new UserFunctionReference(uf)
    }

    foo(loader, element)
  })
  eMap.put("instance", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val body = loader.getFirstChildExpression(element)
      val `type` = loader.parseAlphaCode(element, "of")
      new InstanceOfExpression(body, `type`)
    }

    foo(loader, element)
  })
  eMap.put("int", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val i = new BigInteger(element.getAttributeValue("", "val"))
      Literal.makeLiteral(IntegerValue.makeIntegerValue(i))
    }

    foo(loader, element)
  })
  eMap.put("intersect", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val lhs = loader.getFirstChildExpression(element)
      val rhs = loader.getSecondChildExpression(element)
      new VennExpression(lhs, Token.INTERSECT, rhs)
    }

    foo(loader, element)
  })
  eMap.put("intRangeTest", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val `val` = loader.getFirstChildExpression(element)
      val min = loader.getSecondChildExpression(element)
      val max = loader.getNthChildExpression(element, 2)
      new IntegerRangeTest(`val`, min, max)
    }

    foo(loader, element)
  })
  eMap.put("is", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val lhs = loader.getFirstChildExpression(element)
      val rhs = loader.getSecondChildExpression(element)
      new IdentityComparison(lhs, Token.IS, rhs)
    }

    foo(loader, element)
  })
  eMap.put("isLast", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val cond = element.getAttributeValue("", "test") == "1"
      new IsLastExpression(cond)
    }

    foo(loader, element)
  })
  eMap.put("iterate", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val select = loader.getExpressionWithRole(element, "select")
      val params = loader.getExpressionWithRole(element, "params").asInstanceOf[LocalParamBlock]
      val onCompletion = loader.getExpressionWithRole(element, "on-completion")
      val action = loader.getExpressionWithRole(element, "action")
      new IterateInstr(select, params, action, onCompletion)
    }

    foo(loader, element)
  })
  eMap.put("lastOf", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val base = loader.getFirstChildExpression(element)
      new LastItemExpression(base)
    }

    foo(loader, element)
  })
  eMap.put("let", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val select = loader.getFirstChildExpression(element)
      val slot = loader.getIntegerAttribute(element, "slot")
      val evalMode = loader.getIntegerAttribute(element, "eval")
      val name = loader.getQNameAttribute(element, "var")
      val requiredType = loader.parseAlphaCode(element, "as")
      val let = new LetExpression
      let.setSequence(select)
      let.setRequiredType(requiredType)
      let.setSlotNumber(slot)
      let.setVariableQName(name)
      let.setEvaluationMode(EvaluationMode.forCode(evalMode))
      loader.localBindings.push(let)
      val action = loader.getSecondChildExpression(element)
      loader.localBindings.pop
      let.setAction(action)
      let
    }

    foo(loader, element)
  })
  eMap.put("literal", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val children = new ArrayList[Item]
      val iter = element.iterateAxis(AxisInfo.CHILD, NodeKindTest.ELEMENT)
      var child: NodeInfo = null
      while ( {
        (child = iter.next) != null
      }) {
        val e = loader.loadExpression(child)
        children.add(e.asInstanceOf[Literal].getValue.head)
      }
      Literal.makeLiteral(SequenceExtent.makeSequenceExtent(children))
    }

    foo(loader, element)
  })
  eMap.put("lookup", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val select = loader.getFirstChildExpression(element)
      val key = loader.getSecondChildExpression(element)
      new LookupExpression(select, key)
    }

    foo(loader, element)
  })
  eMap.put("lookupAll", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val select = loader.getFirstChildExpression(element)
      new LookupAllExpression(select)
    }

    foo(loader, element)
  })
  eMap.put("map", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val children = getChildExpressionList(loader, element)
      var key: AtomicValue = null
      val map = new HashTrieMap

      for (child <- children.asScala) {
        if (key == null) key = child.asInstanceOf[Literal].getValue.asInstanceOf[AtomicValue]
        else {
          val value = child.asInstanceOf[Literal].getValue
          map.initialPut(key, value)
          key = null
        }
      }
      Literal.makeLiteral(map)
    }

    foo(loader, element)
  })
  eMap.put("merge", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val inst = new MergeInstr
      val kids = element.iterateAxis(AxisInfo.CHILD, new NameTest(Type.ELEMENT, NamespaceConstant.SAXON_XSLT_EXPORT, "mergeSrc", loader.config.getNamePool))
      var msElem: NodeInfo = null
      val list = new ArrayList[MergeInstr.MergeSource]
      while ( {
        (msElem = kids.next) != null
      }) {
        val ms = new MergeInstr.MergeSource(inst)
        val mergeSourceName = msElem.getAttributeValue("", "name")
        if (mergeSourceName != null) ms.sourceName = mergeSourceName
        val valAtt = msElem.getAttributeValue("", "validation")
        if (valAtt != null) ms.validation = Validation.getCode(valAtt)
        val schemaType = loader.getTypeAttribute(msElem, "type")
        if (schemaType != null) {
          ms.schemaType = schemaType
          ms.validation = Validation.BY_TYPE
        }
        val flagsAtt = msElem.getAttributeValue("", "flags")
        ms.streamable = "s" == flagsAtt
        if (ms.streamable) loader.addCompletionAction(() => ms.prepareForStreaming)
        val rsc = loader.makeRetainedStaticContext(element)
        ms.baseURI = rsc.getStaticBaseUriString
        var accumulatorNames = msElem.getAttributeValue("", "accum")
        if (accumulatorNames == null) accumulatorNames = ""
        val accNameList = new ArrayList[StructuredQName]
        val tokenizer = new StringTokenizer(accumulatorNames)
        while ( {
          tokenizer.hasMoreTokens
        }) {
          val token = tokenizer.nextToken
          val name = StructuredQName.fromEQName(token)
          accNameList.add(name)
        }
        loader.addCompletionAction(new Action() {
          final private[trans] val pack = loader.getPackStack.peek

          override

          def doAction() = {
            val list = new HashSet[Accumulator]

            for (sn <- accNameList.asScala) {

              for (test <- pack.getAccumulatorRegistry.getAllAccumulators.asScala) {
                if (test.getAccumulatorName == sn) list.add(test)
              }
            }
            ms.accumulators = list
          }
        })
        val forEachItem = loader.getExpressionWithRole(msElem, "forEachItem")
        if (forEachItem != null) ms.initForEachItem(inst, forEachItem)
        val forEachStream = loader.getExpressionWithRole(msElem, "forEachStream")
        if (forEachStream != null) ms.initForEachStream(inst, forEachStream)
        val selectRows = loader.getExpressionWithRole(msElem, "selectRows")
        if (selectRows != null) ms.initRowSelect(inst, selectRows)
        val keys = loader.loadSortKeyDefinitions(msElem)
        ms.setMergeKeyDefinitionSet(keys)
        list.add(ms)
      }
      val mergeAction = loader.getExpressionWithRole(element, "action")
      val mergeSources = list.toArray(new Array[MergeInstr.MergeSource](0))
      inst.init(mergeSources, mergeAction)
      loader.completionActions.add(() => inst.fixupGroupReferences)
      inst
    }

    foo(loader, element)
  })
  eMap.put("mergeAdj", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val body = loader.getFirstChildExpression(element)
      new AdjacentTextNodeMerger(body)
    }

    foo(loader, element)
  })
  eMap.put("message", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val select = loader.getExpressionWithRole(element, "select")
      val terminate = loader.getExpressionWithRole(element, "terminate")
      val error = loader.getExpressionWithRole(element, "error")
      //new Message(select, terminate, error)
      null
    }

    foo(loader, element)
  })
  eMap.put("minus", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val body = loader.getFirstChildExpression(element)
      new NegateExpression(body)
    }

    foo(loader, element)
  })
  eMap.put("namespace", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val name = loader.getFirstChildExpression(element)
      val select = loader.getSecondChildExpression(element)
      val inst = new NamespaceConstructor(name)
      inst.setSelect(select)
      inst
    }

    foo(loader, element)
  })
  eMap.put("nextIteration", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val inst = new NextIteration
      val kids = element.iterateAxis(AxisInfo.CHILD, new NameTest(Type.ELEMENT, NamespaceConstant.SAXON_XSLT_EXPORT,
        "withParam", loader.config.getNamePool))
      var wp: NodeInfo = null
      val params = new ArrayList[WithParam]
      while ( {
        (wp = kids.next) != null
      }) {
        val withParam = new WithParam
        val flags = wp.getAttributeValue("", "flags")
        val paramName = loader.getQNameAttribute(wp, "name")
        withParam.setVariableQName(paramName)
        val slot = loader.getIntegerAttribute(wp, "slot")
        withParam.setSlotNumber(slot)
        withParam.setRequiredType(SequenceType.ANY_SEQUENCE)
        withParam.setSelectExpression(inst, loader.getFirstChildExpression(wp))
        withParam.setRequiredType(loader.parseAlphaCode(wp, "as"))
        withParam.setTypeChecked(flags != null && flags.contains("c"))
        params.add(withParam)
      }
      inst.setParameters(params.toArray(new Array[WithParam](0)))
      inst
    }

    foo(loader, element)
  })
  eMap.put("nextMatch", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val flags = element.getAttributeValue("", "flags")
      var useTailRecursion = false
      if (flags != null && flags.contains("t")) useTailRecursion = true
      val inst = new NextMatch(useTailRecursion)
      val actuals = loader.loadWithParams(element, inst, false)
      val tunnels = loader.loadWithParams(element, inst, true)
      inst.setActualParams(actuals)
      inst.setTunnelParams(tunnels)
      inst
    }

    foo(loader, element)
  })
  eMap.put("node", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val kind = loader.getIntegerAttribute(element, "kind")
      val content = element.getAttributeValue("", "content")
      val baseURI = element.getAttributeValue("", "baseURI")
      var node: NodeInfo = null
      kind match {
        case Type.DOCUMENT =>
        case Type.ELEMENT =>
          val source = new StreamSource(new StringReader(content), baseURI)
          node = loader.config.buildDocumentTree(source).getRootNode
          if (kind == Type.ELEMENT) node = VirtualCopy.makeVirtualCopy(node.iterateAxis(AxisInfo.CHILD, NodeKindTest.ELEMENT).next)
        case Type.TEXT =>
        case Type.COMMENT =>
          val o = new Orphan(loader.getConfiguration)
          o.setNodeKind(kind.toShort)
          o.setStringValue(content)
          node = o
        case _ =>
          val o = new Orphan(loader.getConfiguration)
          o.setNodeKind(kind.toShort)
          o.setStringValue(content)
          val prefix = element.getAttributeValue("", "prefix")
          val ns = element.getAttributeValue("", "ns")
          val local = element.getAttributeValue("", "localName")
          if (local != null) {
            val name = new FingerprintedQName(if (prefix == null) ""
            else prefix, if (ns == null) ""
            else ns, local)
            o.setNodeName(name)
          }
          node = o
      }
      Literal.makeLiteral(new One(node))
    }

    foo(loader, element)
  })
  eMap.put("nodeNum", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val levelAtt = element.getAttributeValue("", "level")
      val level = getLevelCode(levelAtt)
      val select = loader.getExpressionWithRole(element, "select")
      val count = loader.getPatternWithRole(element, "count")
      val from = loader.getPatternWithRole(element, "from")
      new NumberInstruction(select, level, count, from)
    }

    foo(loader, element)
  })
  eMap.put("numSeqFmt", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val value = loader.getExpressionWithRole(element, "value")
      var format = loader.getExpressionWithRole(element, "format")
      if (format == null) format = new StringLiteral("1")
      val groupSize = loader.getExpressionWithRole(element, "gpSize")
      val groupSeparator = loader.getExpressionWithRole(element, "gpSep")
      val letterValue = loader.getExpressionWithRole(element, "letterValue")
      val ordinal = loader.getExpressionWithRole(element, "ordinal")
      val startAt = loader.getExpressionWithRole(element, "startAt")
      val lang = loader.getExpressionWithRole(element, "lang")
      val flags = element.getAttributeValue("", "flags")
      val backwardsCompatible = flags != null && flags.contains("1")
      val formatter = null // gets initialized by the NumberSequenceFormatter when possible
      val ni = new NumberSequenceFormatter(value, format, groupSize, groupSeparator, letterValue, ordinal, startAt, lang, formatter, backwardsCompatible)
      ni.preallocateNumberer(loader.config)
      ni
    }

    foo(loader, element)
  })
  eMap.put("onEmpty", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val base = loader.getFirstChildExpression(element)
      new OnEmptyExpr(base)
    }

    foo(loader, element)
  })
  eMap.put("onNonEmpty", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val base = loader.getFirstChildExpression(element)
      new OnNonEmptyExpr(base)
    }

    foo(loader, element)
  })
  eMap.put("or", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val lhs = loader.getFirstChildExpression(element)
      val rhs = loader.getSecondChildExpression(element)
      new OrExpression(lhs, rhs)
    }

    foo(loader, element)
  })
  eMap.put("origF", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val name = loader.getQNameAttribute(element, "name")
      val packKey = element.getAttributeValue("", "pack")
      val declPack = loader.allPackages.get(packKey)
      if (declPack == null) throw new XPathException("Unknown package key " + packKey)
      val arity = loader.getIntegerAttribute(element, "arity")
      val sn = new SymbolicName.F(name, arity)
      val target = declPack.getComponent(sn)
      val orig = new OriginalFunction(target)
      new FunctionLiteral(orig)
    }

    foo(loader, element)
  })
  eMap.put("origFC", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val name = loader.getQNameAttribute(element, "name")
      val packKey = element.getAttributeValue("", "pack")
      val declPack = loader.allPackages.get(packKey)
      if (declPack == null) throw new XPathException("Unknown package key " + packKey)
      val args = getChildExpressionArray(loader, element)
      val arity = args.length
      val sn = new SymbolicName.F(name, arity)
      val target = declPack.getComponent(sn)
      val orig = new OriginalFunction(target)
      new StaticFunctionCall(orig, args)
    }

    foo(loader, element)
  })
  eMap.put("param", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val name = loader.getQNameAttribute(element, "name")
      val slot = loader.getIntegerAttribute(element, "slot")
      val param = new LocalParam
      param.setVariableQName(name)
      param.setSlotNumber(slot)
      val select = loader.getExpressionWithRole(element, "select")
      if (select != null) {
        param.setSelectExpression(select)
        param.computeEvaluationMode()
      }
      val convert = loader.getExpressionWithRole(element, "conversion")
      if (convert != null) param.setConversion(convert)
      param.setRequiredType(loader.parseAlphaCode(element, "as"))
      val flags = element.getAttributeValue("", "flags")
      if (flags != null) {
        param.setTunnel(flags.contains("t"))
        param.setRequiredParam(flags.contains("r"))
        param.setImplicitlyRequiredParam(flags.contains("i"))
      }
      loader.localBindings.add(param)
      param
    }

    foo(loader, element)
  })
  eMap.put("params", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val children = new ArrayList[LocalParam]
      val iter = element.iterateAxis(AxisInfo.CHILD, NodeKindTest.ELEMENT)
      var child: NodeInfo = null
      while ( {
        (child = iter.next) != null
      }) children.add(loader.loadExpression(child).asInstanceOf[LocalParam])
      new LocalParamBlock(children.toArray(new Array[LocalParam](0)))
    }

    foo(loader, element)
  })
  eMap.put("partialApply", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      var count = Count.count(element.iterateAxis(AxisInfo.CHILD, NodeKindTest.ELEMENT))
      var base: Expression = null
      val args = new Array[Expression](count - 1)
      count = 0

      for (child <- element.children(NodeKindTest.ELEMENT)) {
        if (count == 0) base = loader.loadExpression(child)
        else if (child.getLocalPart == "null") args(count - 1) = null
        else args(count - 1) = loader.loadExpression(child)
        count += 1
      }
      new PartialApply(base, args)
    }

    foo(loader, element)
  })
  eMap.put("precedes", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val lhs = loader.getFirstChildExpression(element)
      val rhs = loader.getSecondChildExpression(element)
      new IdentityComparison(lhs, Token.PRECEDES, rhs)
    }

    foo(loader, element)
  })
  eMap.put("procInst", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val name = loader.getFirstChildExpression(element)
      val select = loader.getSecondChildExpression(element)
      val inst = new ProcessingInstruction(name)
      inst.setSelect(select)
      inst
    }

    foo(loader, element)
  })
  eMap.put("qName", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val preAtt = element.getAttributeValue("", "pre")
      val uriAtt = element.getAttributeValue("", "uri")
      val locAtt = element.getAttributeValue("", "loc")
      var `type`: AtomicType = BuiltInAtomicType.QNAME
      if (element.getAttributeValue("", "type") != null) `type` = loader.parseItemTypeAttribute(element, "type").asInstanceOf[AtomicType]
      var `val`: QualifiedNameValue = null
      if (`type`.getPrimitiveType == StandardNames.XS_QNAME) `val` = new QNameValue(preAtt, uriAtt, locAtt, `type`, false)
      else {
        `val` = new NotationValue(preAtt, uriAtt, locAtt, null)
        `val`.setTypeLabel(`type`)
      }
      Literal.makeLiteral(`val`)
    }

    foo(loader, element)
  })
  eMap.put("range", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val from = loader.getIntegerAttribute(element, "from")
      val to = loader.getIntegerAttribute(element, "to")
      Literal.makeLiteral(new IntegerRange(from, to))
    }

    foo(loader, element)
  })
  eMap.put("resultDoc", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      loader.packStack.peek.setCreatesSecondaryResultDocuments(true)
      var href: Expression = null
      var format: Expression = null
      var content: Expression = null
      val globalProps = element.getAttributeValue("", "global")
      val localProps = element.getAttributeValue("", "local")
      val globals = if (globalProps == null) new Properties
      else loader.importProperties(globalProps)
      val locals = if (localProps == null) new Properties
      else loader.importProperties(localProps)
      val dynamicProperties = new HashMap[StructuredQName, Expression]
      var child: NodeInfo = null
      val iter = element.iterateAxis(AxisInfo.CHILD, NodeKindTest.ELEMENT)
      while ( {
        (child = iter.next) != null
      }) {
        val exp = loader.loadExpression(child)
        val role = child.getAttributeValue("", "role")
        if ("href" == role) href = exp
        else if ("format" == role) format = exp
        else if ("content" == role) content = exp
        else {
          val name = StructuredQName.fromEQName(role)
          dynamicProperties.put(name, exp)
        }
      }
      var validation = Validation.SKIP
      val valAtt = element.getAttributeValue("", "validation")
      if (valAtt != null) validation = Validation.getCode(valAtt)
      var schemaType: SchemaType = null
      val typeAtt = loader.getQNameAttribute(element, "type")
      if (typeAtt != null) {
        schemaType = loader.config.getSchemaType(typeAtt)
        validation = Validation.BY_TYPE
      }
      val instr = new ResultDocument(globals, locals, href, format, validation, schemaType, dynamicProperties, loader.packStack.peek.getCharacterMapIndex)
      instr.setContentExpression(content)
      instr
    }

    foo(loader, element)
  })
  eMap.put("root", (loader: PackageLoaderHE, element: NodeInfo) => new RootExpression)
  eMap.put("saxonDoctype", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val arg = loader.getFirstChildExpression(element)
      new Doctype(arg)
    }

    foo(loader, element)
  })
  eMap.put("sequence", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val args = getChildExpressionArray(loader, element)
      new Block(args)
    }

    foo(loader, element)
  })
  eMap.put("slash", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val lhs = loader.getFirstChildExpression(element)
      val rhs = loader.getSecondChildExpression(element)
      val simpleAtt = element.getAttributeValue("", "simple")
      if ("1" == simpleAtt) new SimpleStepExpression(lhs, rhs)
      else {
        val se = new SlashExpression(lhs, rhs)
        if ("2" == simpleAtt) se.setContextFree(true)
        se
      }
    }

    foo(loader, element)
  })
  eMap.put("some", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val select = loader.getFirstChildExpression(element)
      val slot = loader.getIntegerAttribute(element, "slot")
      val name = loader.getQNameAttribute(element, "var")
      val requiredType = loader.parseAlphaCode(element, "as")
      val qEx = new QuantifiedExpression
      qEx.setOperator(Token.SOME)
      qEx.setSequence(select)
      qEx.setRequiredType(requiredType)
      qEx.setSlotNumber(slot)
      qEx.setVariableQName(name)
      loader.localBindings.push(qEx)
      val action = loader.getSecondChildExpression(element)
      loader.localBindings.pop
      qEx.setAction(action)
      qEx
    }

    foo(loader, element)
  })
  eMap.put("sort", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val body = loader.getFirstChildExpression(element)
      val sortKeys = loader.loadSortKeyDefinitions(element)
      new SortExpression(body, sortKeys)
    }

    foo(loader, element)
  })
  eMap.put("sourceDoc", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val valSpecified = loader.getIntegerAttribute(element, "validation")
      var validation = if (valSpecified == Integer.MIN_VALUE) Validation.SKIP
      else valSpecified
      var schemaType: SchemaType = null
      val typeAtt = loader.getQNameAttribute(element, "schemaType")
      if (typeAtt != null) {
        schemaType = loader.getConfiguration.getSchemaType(typeAtt)
        validation = Validation.BY_TYPE
      }
      val options = new ParseOptions(loader.getConfiguration.getParseOptions)
      options.setSchemaValidationMode(validation)
      options.setTopLevelType(schemaType)
      val flags = element.getAttributeValue("", "flags")
      if (flags != null) {
        if (flags.contains("s")) loader.addCompletionAction(() => options.setSpaceStrippingRule(loader.getPackage.getSpaceStrippingRule))
        if (flags.contains("l")) options.setLineNumbering(true)
        if (flags.contains("a")) options.setExpandAttributeDefaults(true)
        if (flags.contains("d")) options.setDTDValidationMode(Validation.STRICT)
        if (flags.contains("i")) options.setXIncludeAware(true)
      }
      val body = loader.getExpressionWithRole(element, "body")
      val href = loader.getExpressionWithRole(element, "href")
      val inst = new SourceDocument(href, body, options)
      val accumulatorNames = element.getAttributeValue("", "accum")
      processAccumulatorList(loader, inst, accumulatorNames)
      inst
    }

    foo(loader, element)
  })
  eMap.put("str", (loader: PackageLoaderHE, element: NodeInfo) => Literal.makeLiteral(new StringValue(element.getAttributeValue("", "val"))))
  eMap.put("subscript", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val lhs = loader.getFirstChildExpression(element)
      val rhs = loader.getSecondChildExpression(element)
      new SubscriptExpression(lhs, rhs)
    }

    foo(loader, element)
  })
  eMap.put("supplied", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val slot = loader.getIntegerAttribute(element, "slot")
      new SuppliedParameterReference(slot)
    }

    foo(loader, element)
  })
  eMap.put("tail", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val select = loader.getFirstChildExpression(element)
      val start = loader.getIntegerAttribute(element, "start")
      new TailExpression(select, start)
    }

    foo(loader, element)
  })
  eMap.put("tailCallLoop", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val body = loader.getFirstChildExpression(element)
      new TailCallLoop(loader.currentFunction, body)
    }

    foo(loader, element)
  })
  eMap.put("to", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val lhs = loader.getFirstChildExpression(element)
      val rhs = loader.getSecondChildExpression(element)
      new RangeExpression(lhs, rhs)
    }

    foo(loader, element)
  })
  eMap.put("treat", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val body = loader.getFirstChildExpression(element)
      val `type` = loader.parseAlphaCodeForItemType(element, "as")
      val role = RoleDiagnostic.reconstruct(element.getAttributeValue("", "diag"))
      new ItemChecker(body, `type`, role)
    }

    foo(loader, element)
  })
  eMap.put("true", (loader: PackageLoaderHE, element: NodeInfo) => Literal.makeLiteral(BooleanValue.TRUE))
  eMap.put("try", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val tryExp = loader.getFirstChildExpression(element)
      val tryCatch = new TryCatch(tryExp)
      if ("r" == element.getAttributeValue("", "flags")) tryCatch.setRollbackOutput(true)
      val iter = element.iterateAxis(AxisInfo.CHILD, new NameTest(Type.ELEMENT, NamespaceConstant.SAXON_XSLT_EXPORT, "catch", loader.config.getNamePool))
      var catchElement: NodeInfo = null
      val pool = loader.getConfiguration.getNamePool
      while ( {
        (catchElement = iter.next) != null
      }) {
        val errAtt = catchElement.getAttributeValue("", "errors")
        val tests = errAtt.split(" ")
        val list = new ArrayList[QNameTest]
        for (t <- tests) {
          if (t == "*") list.add(AnyNodeTest.getInstance)
          else if (t.startsWith("*:")) list.add(new LocalNameTest(pool, Type.ELEMENT, t.substring(2)))
          else if (t.endsWith("}*")) list.add(new NamespaceTest(pool, Type.ELEMENT, t.substring(2, t.length - 2)))
          else {
            val qName = StructuredQName.fromEQName(t)
            list.add(new NameTest(Type.ELEMENT, new FingerprintedQName(qName, pool), pool))
          }
        }
        var test: QNameTest = null
        if (list.size == 1) test = list.get(0)
        else test = new UnionQNameTest(list)
        val catchExpr = loader.getFirstChildExpression(catchElement)
        tryCatch.addCatchExpression(test, catchExpr)
      }
      tryCatch
    }

    foo(loader, element)
  })

  import EvaluationMode._

  eMap.put("ufCall", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val args = getChildExpressionArray(loader, element)
      val name = loader.getQNameAttribute(element, "name")
      val call = new UserFunctionCall
      call.setFunctionName(name)
      call.setArguments(args)
      val bindingSlot = loader.getIntegerAttribute(element, "bSlot")
      call.setBindingSlot(bindingSlot)
      val eval = element.getAttributeValue("", "eval")
      if (eval != null) {
        val evals = eval.split(" ")
        val evalModes = new Array[EvaluationMode](evals.length)
        for (i <- 0 until evals.length) {
          evalModes(i) = EvaluationMode.forCode(evals(i).toInt)
        }
        call.setArgumentEvaluationModes(evalModes)
      }
      loader.fixups.peek.add(call)
      call
    }

    foo(loader, element)
  })
  eMap.put("ufRef", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val name = loader.getQNameAttribute(element, "name")
      val arity = loader.getIntegerAttribute(element, "arity")
      val symbolicName = new SymbolicName.F(name, arity)
      val call = new UserFunctionReference(symbolicName)
      val bindingSlot = loader.getIntegerAttribute(element, "bSlot")
      call.setBindingSlot(bindingSlot)
      loader.fixups.peek.add(call)
      call
    }

    foo(loader, element)
  })
  eMap.put("union", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val lhs = loader.getFirstChildExpression(element)
      val rhs = loader.getSecondChildExpression(element)
      new VennExpression(lhs, Token.UNION, rhs)
    }

    foo(loader, element)
  })
  eMap.put("useAS", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val name = loader.getQNameAttribute(element, "name")
      val streamable = "s" == element.getAttributeValue("", "flags")
      val use = new UseAttributeSet(name, streamable)
      val bindingSlot = loader.getIntegerAttribute(element, "bSlot")
      use.setBindingSlot(bindingSlot)
      loader.fixups.peek.add(use)
      use
    }

    foo(loader, element)
  })
  eMap.put("valueOf", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val select = loader.getFirstChildExpression(element)
      val flags = element.getAttributeValue("", "flags")
      val doe = flags != null && flags.contains("d")
      val notIfEmpty = flags != null && flags.contains("e")
      new ValueOf(select, doe, notIfEmpty)
    }

    foo(loader, element)
  })
  eMap.put("varRef", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val name = loader.getQNameAttribute(element, "name")
      val locals = loader.localBindings
      var binding: LocalBinding = null
      breakable {
        for (i <- locals.size - 1 to 0 by -1) {
          val b = locals.get(i)
          if (b.getVariableQName == name) {
            binding = b
            break //todo: break is not supported
          }
        }
      }
      if (binding == null) throw new XPathException("No binding found for local variable " + name)
      val slot = loader.getIntegerAttribute(element, "slot")
      val ref = new LocalVariableReference(binding)
      ref.setSlotNumber(slot)
      ref
    }

    foo(loader, element)
  })
  eMap.put("vc", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val opAtt = element.getAttributeValue("", "op")
      var op = 0
      op = parseValueComparisonOperator(opAtt)
      val lhs = loader.getFirstChildExpression(element)
      val rhs = loader.getSecondChildExpression(element)
      val vc = new ValueComparison(lhs, op, rhs)
      val compAtt = element.getAttributeValue("", "comp")
      val comp = loader.makeAtomicComparer(compAtt, element)
      vc.setAtomicComparer(comp)
      val onEmptyAtt = element.getAttributeValue("", "onEmpty")
      if (onEmptyAtt != null) vc.setResultWhenEmpty(BooleanValue.get("1" == onEmptyAtt))
      vc
    }

    foo(loader, element)
  })
  pMap.put("p.anchor", (loader: PackageLoaderHE, element: NodeInfo) => AnchorPattern.getInstance)
  pMap.put("p.any", (loader: PackageLoaderHE, element: NodeInfo) => new UniversalPattern)
  pMap.put("p.booleanExp", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val exp = loader.getFirstChildExpression(element)
      new BooleanExpressionPattern(exp)
    }

    foo(loader, element)
  })
  pMap.put("p.genNode", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val `type` = loader.parseAlphaCodeForItemType(element, "test").asInstanceOf[NodeTest]
      val exp = loader.getFirstChildExpression(element)
      new GeneralNodePattern(exp, `type`)
    }

    foo(loader, element)
  })
  pMap.put("p.genPos", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val `type` = loader.parseAlphaCodeForItemType(element, "test").asInstanceOf[NodeTest]
      val exp = loader.getFirstChildExpression(element)
      val flags = element.getAttributeValue("", "flags")
      val gpp = new GeneralPositionalPattern(`type`, exp)
      gpp.setUsesPosition(!("P" == flags))
      gpp
    }

    foo(loader, element)
  })
  pMap.put("p.nodeSet", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val `type` = loader.parseAlphaCodeForItemType(element, "test")
      val select = loader.getFirstChildExpression(element)
      val pat = new NodeSetPattern(select)
      pat.setItemType(`type`)
      pat
    }

    foo(loader, element)
  })
  pMap.put("p.nodeTest", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val test = loader.parseAlphaCodeForItemType(element, "test")
      if (test.isInstanceOf[NodeTest]) new NodeTestPattern(test.asInstanceOf[NodeTest])
      else new ItemTypePattern(test)
    }

    foo(loader, element)
  })
  pMap.put("p.venn", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val p0 = loader.getFirstChildPattern(element)
      val p1 = loader.getSecondChildPattern(element)
      val operator = element.getAttributeValue("", "op")
      operator match {
        case "union" =>
          new UnionPattern(p0, p1)
        case "intersect" =>
          new IntersectPattern(p0, p1)
        case "except" =>
          new ExceptPattern(p0, p1)
      }
      null
    }

    foo(loader, element)
  })
  pMap.put("p.simPos", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val test = loader.parseAlphaCodeForItemType(element, "test").asInstanceOf[NodeTest]
      val pos = loader.getIntegerAttribute(element, "pos")
      new SimplePositionalPattern(test, pos)
    }

    foo(loader, element)
  })
  pMap.put("p.withCurrent", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val let = new LocalVariableBinding(Current.FN_CURRENT, SequenceType.SINGLE_ITEM)
      let.setSlotNumber(0)
      loader.localBindings.push(let)
      val p0 = loader.getFirstChildPattern(element)
      loader.localBindings.pop
      new PatternThatSetsCurrent(p0, let)
    }

    foo(loader, element)
  })
  pMap.put("p.withUpper", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val axisName = element.getAttributeValue("", "axis")
      val axis = AxisInfo.getAxisNumber(axisName)
      val basePattern = loader.getFirstChildPattern(element)
      val upperPattern = loader.getSecondChildPattern(element)
      new AncestorQualifiedPattern(basePattern, upperPattern, axis)
    }

    foo(loader, element)
  })
  pMap.put("p.withPredicate", (loader: PackageLoaderHE, element: NodeInfo) => {
    def foo(loader: PackageLoaderHE, element: NodeInfo) = {
      val basePattern = loader.getFirstChildPattern(element)
      val predicate = loader.getSecondChildExpression(element)
      new BasePatternWithPredicate(basePattern, predicate)
    }

    foo(loader, element)
  })
}

class PackageLoaderHE(var config: Configuration) extends IPackageLoader {
  overriding = new ExecutableFunctionLibrary(config)
  underriding = new ExecutableFunctionLibrary(config)
  try {
    parser = config.newExpressionParser("XP", false, 31)
    val qNameParser = new QNameParser(null).withAcceptEQName(true)
    parser.setQNameParser(qNameParser)
  } catch {
    case e: XPathException =>
      throw new AssertionError(null)
  }

  val packStack: Stack[StylesheetPackage] = new Stack()

  private var parser: XPathParser = _

  val fixups: Stack[List[ComponentInvocation]] = new Stack()

  val completionActions: List[Action] = new ArrayList()

  val allPackages: Map[String, StylesheetPackage] = new HashMap()

  var localBindings: Stack[LocalBinding] = _

  private var overriding: ExecutableFunctionLibrary = _

  private var underriding: ExecutableFunctionLibrary = _

  private val contextStack: Stack[RetainedStaticContext] = new Stack()

  val userFunctions: Map[SymbolicName, UserFunction] = new HashMap()

  private val locationMap: Map[String, IntHashMap[Location]] = new HashMap()

  private val componentIdMap: Map[Integer, Component] = new HashMap()

  private val externalReferences: Map[Component, String] = new HashMap()

  private var relocatableBase: String = null

  private var originalVersion: NestedIntegerValue = null

  def getConfiguration = config

  def getPackage = packStack.get(0)

  def getPackStack = packStack

  def addCompletionAction(action: Action): Boolean = completionActions.add(action)

  @throws[XPathException]
  override def loadPackage(source: Source): StylesheetPackage = {
    val options = new ParseOptions
    options.setSpaceStrippingRule(AllElementsSpaceStrippingRule.getInstance)
    options.setSchemaValidationMode(Validation.SKIP)
    options.setDTDValidationMode(Validation.SKIP)
    val filters = new ArrayList[ProxyReceiver](1)
    val checksumFactory = new FilterFactory() {
      /**
       * Make a ProxyReceiver to filter events on a push pipeline
       *
       * @param next the next receiver in the pipeline
       * @return a ProxyReceiver initialized to send events to the next receiver in the pipeine
       */
      override def makeFilter(next: Receiver) = {
        val filter = new CheckSumFilter(next)
        filter.setCheckExistingChecksum(true)
        filters.add(filter)
        filter
      }
    }
    options.addFilter(checksumFactory)
    val doc = config.buildDocumentTree(source, options).getRootNode
    val csf = filters.get(0).asInstanceOf[CheckSumFilter]
    if (!csf.isChecksumCorrect) throw new XPathException("Package cannot be loaded: incorrect checksum", SaxonErrorCode.SXPK0002)
    loadPackageDoc(doc)
    null
  }

  @throws[XPathException]
  override def loadPackageDoc(doc: NodeInfo) = {
    /* val pack = config.makeStylesheetPackage
     pack.setRuleManager(new RuleManager(pack))
     pack.setCharacterMapIndex(new CharacterMapIndex)
     pack.setJustInTimeCompilation(false)*/
    //packStack.push(pack)
    val packageElement = doc.iterateAxis(AxisInfo.CHILD, NodeKindTest.ELEMENT).next
    if (!(packageElement.getURI == NamespaceConstant.SAXON_XSLT_EXPORT)) throw new XPathException("Incorrect namespace for XSLT export file", SaxonErrorCode.SXPK0002)
    if (!(packageElement.getLocalPart == "package")) throw new XPathException("Outermost element of XSLT export file must be 'package'", SaxonErrorCode.SXPK0002)
    var saxonVersionAtt = packageElement.getAttributeValue("", "saxonVersion")
    if (saxonVersionAtt == null) saxonVersionAtt = "9.8.0.1" //Arbitrarily; older SEF files do not have this attribute
    originalVersion = NestedIntegerValue.parse(saxonVersionAtt)
    val dmk = packageElement.getAttributeValue("", "dmk")
    if (dmk != null) {
      val licenseId = config.registerLocalLicense(dmk)
      //pack.setLocalLicenseId(licenseId)
    }
    // loadPackageElement(packageElement, pack)

    for (entry <- externalReferences.entrySet.asScala) {
      val comp = entry.getKey
      val tokenizer = new StringTokenizer(entry.getValue)
      while ( {
        tokenizer.hasMoreTokens
      }) {
        val token = tokenizer.nextToken
        val target = token.toInt
        val targetComponent = componentIdMap.get(target)
        if (targetComponent == null) throw new XPathException("Unresolved external reference to component " + target)
        comp.getComponentBindings.add(new ComponentBinding(targetComponent.getActor.getSymbolicName, targetComponent))
      }
    }
    null
  }

  def needsPELicense(name: String) = {
    val localLicenseId = getPackage.getLocalLicenseId
    config.checkLicensedFeature(Configuration.LicenseFeature.PROFESSIONAL_EDITION, name, localLicenseId)
  }

  def needsEELicense(name: String) = {
    val localLicenseId = getPackage.getLocalLicenseId
    config.checkLicensedFeature(Configuration.LicenseFeature.ENTERPRISE_XSLT, name, localLicenseId)
  }

  @throws[XPathException]
  def loadPackageElement(packageElement: NodeInfo, pack: StylesheetPackage) = {
    fixups.push(new ArrayList[ComponentInvocation])
    val packageName = packageElement.getAttributeValue("", "name")
    val packageId = packageElement.getAttributeValue("", "id")
    val packageKey = if (packageId == null) packageName
    else packageId // for backwards compatibility with 9.8
    val relocatable = "true" == packageElement.getAttributeValue("", "relocatable")
    if (packageName != null) {
      pack.setPackageName(packageName)
      allPackages.put(packageKey, pack)
    }
    //pack.setPackageVersion(new PackageVersion(packageElement.getAttributeValue("", "packageVersion")))
    pack.setVersion(getIntegerAttribute(packageElement, "version"))
    pack.setSchemaAware("1" == packageElement.getAttributeValue("", "schemaAware"))
    if (pack.isSchemaAware) needsEELicense("schema-awareness")
    val implicitAtt = packageElement.getAttributeValue("", "implicit")
    if (implicitAtt != null) pack.setImplicitPackage(implicitAtt == "true")
    else { // For export files created prior to Saxon 9.9.1.2, we'll treat the package as implicit,
      // for compatibility: otherwise, setInitialTemplate("main") will fail when the main template
      // has no "visibility" attribute
      pack.setImplicitPackage(originalVersion.compareTo(PackageLoaderHE.SAXON9911) <= 0)
    }
    pack.setStripsTypeAnnotations("1" == packageElement.getAttributeValue("", "stripType"))
    //pack.setKeyManager(new KeyManager(pack.getConfiguration, pack))
    pack.setDeclaredModes("1" == packageElement.getAttributeValue("", "declaredModes"))

    /*for (usePack <- packageElement.children(new NameTest(Type.ELEMENT, NamespaceConstant.SAXON_XSLT_EXPORT, "package", config.getNamePool))) {
      /*val subPack = config.makeStylesheetPackage
      subPack.setRuleManager(new RuleManager(pack))
      subPack.setCharacterMapIndex(new CharacterMapIndex)
      subPack.setJustInTimeCompilation(false)*/
     /* packStack.push(subPack)
      loadPackageElement(usePack, subPack)*/
      packStack.pop
      pack.addUsedPackage(subPack)
    }*/
    val functionLibrary = new FunctionLibraryList
    //functionLibrary.addFunctionLibrary(config.getXSLT30FunctionSet)
    functionLibrary.addFunctionLibrary(MapFunctionSet.getInstance)
    functionLibrary.addFunctionLibrary(ArrayFunctionSet.getInstance)
    functionLibrary.addFunctionLibrary(MathFunctionSet.getInstance)
    //functionLibrary.addFunctionLibrary(overriding);
    // functionLibrary.addFunctionLibrary(new StylesheetFunctionLibrary(pack, true))
    functionLibrary.addFunctionLibrary(new ConstructorFunctionLibrary(config))
    val queryFunctions = new XQueryFunctionLibrary(config)
    functionLibrary.addFunctionLibrary(queryFunctions)
    functionLibrary.addFunctionLibrary(config.getIntegratedFunctionLibrary)
    config.addExtensionBinders(functionLibrary)
    //functionLibrary.addFunctionLibrary(underriding);
    // functionLibrary.addFunctionLibrary(new StylesheetFunctionLibrary(pack, false))
    pack.setFunctionLibraryDetails(functionLibrary, overriding, underriding)
    val rsc = new RetainedStaticContext(config)
    if (relocatable) { // For a relocatable package, take the base URI from the location of the SEF file
      relocatableBase = packageElement.getBaseURI
      rsc.setStaticBaseUriString(relocatableBase)
    }
    rsc.setPackageData(pack)
    contextStack.push(rsc)
    localBindings = new Stack[LocalBinding]
    readGlobalContext(packageElement)
    readSchemaNamespaces(packageElement)
    readKeys(packageElement)
    readComponents(packageElement, false)
    val overridden = packageElement.iterateAxis(AxisInfo.CHILD, new NameTest(Type.ELEMENT, NamespaceConstant.SAXON_XSLT_EXPORT, "overridden", config.getNamePool)).next
    if (overridden != null) readComponents(overridden, true)
    readAccumulators(packageElement)
    readOutputProperties(packageElement)
    readCharacterMaps(packageElement)
    readSpaceStrippingRules(packageElement)
    readDecimalFormats(packageElement)
    resolveFixups()
    fixups.pop

    for (a <- completionActions.asScala) {
      a.doAction()
    }
    val defaultModeName = getQNameAttribute(packageElement, "defaultMode")
    if (defaultModeName == null) pack.setDefaultMode(Mode.UNNAMED_MODE_NAME)
    else pack.setDefaultMode(defaultModeName)
  }

  @throws[XPathException]
  private def readGlobalContext(packageElement: NodeInfo) = {
    val req = new GlobalContextRequirement
    packStack.peek.setContextItemRequirements(req)
    val condition = new NameTest(Type.ELEMENT, NamespaceConstant.SAXON_XSLT_EXPORT, "glob", config.getNamePool)

    for (varElement <- packageElement.children(condition)) {
      val use = varElement.getAttributeValue("", "use")
      if ("opt" == use) {
        req.setMayBeOmitted(true)
        req.setAbsentFocus(false)
      }
      else if ("pro" == use) {
        req.setMayBeOmitted(true)
        req.setAbsentFocus(true)
      }
      else if ("req" == use) {
        req.setMayBeOmitted(false)
        req.setAbsentFocus(false)
      }
      val requiredType = parseItemTypeAttribute(varElement, "type")
      if (requiredType != null) req.addRequiredItemType(requiredType)
    }
  }

  @throws[XPathException]
  def readSchemaNamespaces(packageElement: NodeInfo) = {
    // No action in Saxon-HE
  }

  @throws[XPathException]
  private def readKeys(packageElement: NodeInfo) = {
    val pack = packStack.peek
    var keyElement: NodeInfo = null
    val iterator = packageElement.iterateAxis(AxisInfo.CHILD, new NameTest(Type.ELEMENT, NamespaceConstant.SAXON_XSLT_EXPORT, "key", config.getNamePool))
    while ( {
      (keyElement = iterator.next) != null
    }) {
      val keyName = getQNameAttribute(keyElement, "name")
      val symbol = new SymbolicName(StandardNames.XSL_KEY, keyName)
      val flags = keyElement.getAttributeValue("", "flags")
      val backwards = flags != null && flags.contains("b")
      val range = flags != null && flags.contains("r")
      val reusable = flags != null && flags.contains("u")
      val composite = flags != null && flags.contains("c")
      val `match` = getFirstChildPattern(keyElement)
      val use = getSecondChildExpression(keyElement)
      var collationName = keyElement.getAttributeValue("", "collation")
      if (collationName == null) collationName = NamespaceConstant.CODEPOINT_COLLATION_URI
      val collation = config.getCollation(collationName)
      // val keyDefinition = new KeyDefinition(symbol, `match`, use, collationName, collation)
      val slots = getIntegerAttribute(keyElement, "slots")
      // if (slots != Integer.MIN_VALUE) keyDefinition.setStackFrameMap(new SlotManager(slots))
      val binds = keyElement.getAttributeValue("", "binds")
      //val keyComponent = keyDefinition.makeDeclaringComponent(Visibility.PRIVATE, pack)
      // externalReferences.put(keyComponent, binds)
      // if (backwards) keyDefinition.setBackwardsCompatible(true)
      //if (range) keyDefinition.setRangeKey(true)
      //if (composite) keyDefinition.setComposite(true)
      //pack.getKeyManager.addKeyDefinition(keyName, keyDefinition, reusable, pack.getConfiguration)
      //pack.addComponent(keyComponent);
    }
  }

  @throws[XPathException]
  private def readComponents(packageElement: NodeInfo, overridden: Boolean) = {
    val pack = packStack.peek
    var child: NodeInfo = null
    val iterator = packageElement.iterateAxis(AxisInfo.CHILD, new NameTest(Type.ELEMENT, NamespaceConstant.SAXON_XSLT_EXPORT, "co", config.getNamePool))
    while ( {
      (child = iterator.next) != null
    }) {
      val id = getIntegerAttribute(child, "id")
      val visAtt = child.getAttributeValue("", "vis")
      val vis: Visibility.Visibility = if (visAtt == null) Visibility.PRIVATE
      else Visibility.withName(visAtt.toUpperCase)
      val provenance = if (visAtt == null) VisibilityProvenance.DEFAULTED
      else VisibilityProvenance.EXPLICIT
      val binds = child.getAttributeValue("", "binds")
      val dPackKey = child.getAttributeValue("", "dpack")
      var declaringPackage: StylesheetPackage = null
      if (dPackKey == null) declaringPackage = pack
      else if (allPackages.containsKey(dPackKey)) declaringPackage = allPackages.get(dPackKey)
      else {
        //declaringPackage = config.makeStylesheetPackage
        declaringPackage.setPackageName(dPackKey)
        declaringPackage.setTargetEdition(config.getEditionCode)
        declaringPackage.setJustInTimeCompilation(false)
      }
      var component: Component = null
      val base = getIntegerAttribute(child, "base")
      if (base != Integer.MIN_VALUE) { // Note, this cannot be a forwards reference
        val baseComponent = componentIdMap.get(base)
        if (baseComponent == null) throw new AssertionError(base + "")
        component = Component.makeComponent(baseComponent.getActor, vis, provenance, pack, declaringPackage)
        component.setBaseComponent(baseComponent)
        /* if (component.isInstanceOf[Component.M]) { // Create the mode even if there are no mode children: test case override-v-015
           pack.getRuleManager.obtainMode(baseComponent.getActor.getComponentName, true)
         }*/
      }
      else {
        val grandchild = child.iterateAxis(AxisInfo.CHILD, NodeKindTest.ELEMENT).next
        var cc: Actor = null
        val kind = grandchild.getLocalPart
        var codeGen = false
        kind match {
          case "template" =>
            cc = readNamedTemplate(grandchild)
            codeGen = true
          case "globalVariable" =>
            cc = readGlobalVariable(grandchild)
            codeGen = true
          case "globalParam" =>
            cc = readGlobalParam(grandchild)
          case "function" =>
            cc = readGlobalFunction(grandchild)
            codeGen = (cc.asInstanceOf[UserFunction]).getDeclaredStreamability eq FunctionStreamability.UNCLASSIFIED
          case "mode" =>
            cc = readMode(grandchild)
          case "attributeSet" =>
            cc = readAttributeSet(grandchild)
          case _ =>
            throw new XPathException("unknown component kind " + kind)
        }
        component = Component.makeComponent(cc, vis, provenance, pack, declaringPackage)
        cc.setDeclaringComponent(component)
        cc.setDeclaredVisibility(vis)
        val optimizer = config.obtainOptimizer
        val name = cc.getComponentName
        val evaluationModes = Expression.ITERATE_METHOD | Expression.PROCESS_METHOD
        if (codeGen) {
          val objectName = if (name == null) "h" + component.hashCode
          else name.getLocalPart
          cc.setBody(optimizer.makeByteCodeCandidate(cc, cc.getBody, objectName, evaluationModes))
          optimizer.injectByteCodeCandidates(cc.getBody)
        }
        else if (cc.isInstanceOf[Mode]) cc.asInstanceOf[Mode].processRules((rule: Rule) => {
          def foo(rule: Rule) = {
            val tr = rule.getAction.asInstanceOf[TemplateRule]
            val objectName = "match=\"" + tr.getMatchPattern + '"'
            tr.setBody(optimizer.makeByteCodeCandidate(tr, tr.getBody, objectName, evaluationModes))
            optimizer.injectByteCodeCandidates(tr.getBody)
          }

          foo(rule)
        })
      }
      externalReferences.put(component, binds)
      componentIdMap.put(id, component)
      if (overridden) pack.addOverriddenComponent(component)
      else if (component.getVisibility eq Visibility.HIDDEN) pack.addHiddenComponent(component)
      else pack.addComponent(component)
    }
  }

  @throws[XPathException]
  private def readGlobalVariable(varElement: NodeInfo) = {
    val pack = packStack.peek
    val variableName = getQNameAttribute(varElement, "name")
    val `var` = new GlobalVariable
    `var`.setVariableQName(variableName)
    `var`.setPackageData(pack)
    `var`.setRequiredType(parseAlphaCode(varElement, "as"))
    val flags = varElement.getAttributeValue("", "flags")
    if (flags != null) {
      if (flags.contains("a")) `var`.setAssignable(true)
      if (flags.contains("x")) `var`.setIndexedVariable()
      if (flags.contains("r")) `var`.setRequiredParam(true)
    }
    val slots = getIntegerAttribute(varElement, "slots")
    if (slots > 0) `var`.setContainsLocals(new SlotManager(slots))
    val bodyElement = varElement.iterateAxis(AxisInfo.CHILD, NodeKindTest.ELEMENT).next
    if (bodyElement == null) `var`.setBody(Literal.makeEmptySequence)
    else {
      val body = loadExpression(bodyElement)
      `var`.setBody(body)
      val rsc = body.getRetainedStaticContext
      body.setRetainedStaticContext(rsc) // to propagate it to the subtree
    }
    pack.addGlobalVariable(`var`)
    `var`
  }

  @throws[XPathException]
  private def readGlobalParam(varElement: NodeInfo) = {
    val pack = packStack.peek
    val variableName = getQNameAttribute(varElement, "name")
    //System.err.println("Loading global variable " + variableName);
    localBindings = new Stack[LocalBinding]
    val `var` = new GlobalParam
    `var`.setVariableQName(variableName)
    `var`.setPackageData(pack)
    `var`.setRequiredType(parseAlphaCode(varElement, "as"))
    val flags = varElement.getAttributeValue("", "flags")
    if (flags != null) {
      if (flags.contains("a")) `var`.setAssignable(true)
      if (flags.contains("x")) `var`.setIndexedVariable()
      if (flags.contains("r")) `var`.setRequiredParam(true)
      if (flags.contains("i")) `var`.setImplicitlyRequiredParam(true)
    }
    val slots = getIntegerAttribute(varElement, "slots")
    if (slots > 0) `var`.setContainsLocals(new SlotManager(slots))
    val bodyElement = varElement.iterateAxis(AxisInfo.CHILD, NodeKindTest.ELEMENT).next
    if (bodyElement == null) `var`.setBody(Literal.makeEmptySequence)
    else {
      val body = loadExpression(bodyElement)
      `var`.setBody(body)
      val rsc = body.getRetainedStaticContext
      body.setRetainedStaticContext(rsc)
    }
    `var`
  }

  @throws[XPathException]
  private def readNamedTemplate(templateElement: NodeInfo) = {
    val pack = packStack.peek
    localBindings = new Stack[LocalBinding]
    val templateName = getQNameAttribute(templateElement, "name")
    val flags = templateElement.getAttributeValue("", "flags")
    val slots = getIntegerAttribute(templateElement, "slots")
    val contextType = parseAlphaCode(templateElement, "cxt")
    val contextItemType = if (contextType == null) AnyItemType.getInstance
    else contextType.getPrimaryType
    val template = new NamedTemplate(templateName)
    template.setStackFrameMap(new SlotManager(slots))
    template.setPackageData(pack)
    template.setRequiredType(parseAlphaCode(templateElement, "as"))
    template.setContextItemRequirements(contextItemType, flags.contains("o"), !flags.contains("s"))
    val bodyElement = getChildWithRole(templateElement, "body")
    if (bodyElement == null) template.setBody(Literal.makeEmptySequence)
    else {
      val body = loadExpression(bodyElement)
      template.setBody(body)
      val rsc = body.getRetainedStaticContext
      body.setRetainedStaticContext(rsc)
    }
    template
  }

  @throws[XPathException]
  private def readGlobalFunction(functionElement: NodeInfo) = {
    localBindings = new Stack[LocalBinding]
    val function = readFunction(functionElement)
    userFunctions.put(function.getSymbolicName, function)
    underriding.addFunction(function)
    function
  }

  private var currentFunction: UserFunction = null

  @throws[XPathException]
  def readFunction(functionElement: NodeInfo) = {
    val pack = packStack.peek
    val functionName = getQNameAttribute(functionElement, "name")
    val slots = getIntegerAttribute(functionElement, "slots")
    var flags = functionElement.getAttributeValue("", "flags")
    if (flags == null) flags = ""
    val function: UserFunction = makeFunction(flags)
    function.setFunctionName(functionName)
    function.setStackFrameMap(new SlotManager(slots))
    function.setPackageData(pack)
    function.setRetainedStaticContext(makeRetainedStaticContext(functionElement))
    function.setResultType(parseAlphaCode(functionElement, "as"))
    function.setDeclaredStreamability(FunctionStreamability.UNCLASSIFIED)
    function.incrementReferenceCount() // ensure it's exported in any re-export
    val evalMode = getIntegerAttribute(functionElement, "eval")
    if (flags.contains("p")) function.setDeterminism(UserFunction.Determinism.PROACTIVE)
    else if (flags.contains("e")) function.setDeterminism(UserFunction.Determinism.ELIDABLE)
    else if (flags.contains("d")) function.setDeterminism(UserFunction.Determinism.DETERMINISTIC)
    // Ignore the "m" flag - handled in subclass for Saxon-PE
    var streaming = false
    if (flags.contains("U")) function.setDeclaredStreamability(FunctionStreamability.UNCLASSIFIED)
    else if (flags.contains("A")) {
      function.setDeclaredStreamability(FunctionStreamability.ABSORBING)
      streaming = true
    }
    else if (flags.contains("I")) {
      function.setDeclaredStreamability(FunctionStreamability.INSPECTION)
      streaming = true
    }
    else if (flags.contains("F")) {
      function.setDeclaredStreamability(FunctionStreamability.FILTER)
      streaming = true
    }
    else if (flags.contains("S")) {
      function.setDeclaredStreamability(FunctionStreamability.SHALLOW_DESCENT)
      streaming = true
    }
    else if (flags.contains("D")) {
      function.setDeclaredStreamability(FunctionStreamability.DEEP_DESCENT)
      streaming = true
    }
    else if (flags.contains("C")) {
      function.setDeclaredStreamability(FunctionStreamability.ASCENT)
      streaming = true
    }
    function.setEvaluationMode(EvaluationMode.forCode(evalMode))
    currentFunction = function
    val params = new ArrayList[UserFunctionParameter]
    val argIterator = functionElement.iterateAxis(AxisInfo.CHILD, new NameTest(Type.ELEMENT, NamespaceConstant.SAXON_XSLT_EXPORT, "arg", config.getNamePool))
    var argElement: NodeInfo = null
    var slot = 0
    while ( {
      (argElement = argIterator.next) != null
    }) {
      val arg = new UserFunctionParameter
      arg.setVariableQName(getQNameAttribute(argElement, "name"))
      arg.setRequiredType(parseAlphaCode(argElement, "as"))
      arg.setSlotNumber({
        slot += 1;
        slot - 1
      })
      params.add(arg)
      localBindings.push(arg)
    }
    function.setParameterDefinitions(params.toArray(new Array[UserFunctionParameter](0)))
    if (streaming) params.get(0).setFunctionStreamability(function.getDeclaredStreamability)
    val bodyElement = getChildWithRole(functionElement, "body")
    if (bodyElement == null) function.setBody(Literal.makeEmptySequence)
    else {
      val body = loadExpression(bodyElement)
      function.setBody(body)
      val rsc = body.getRetainedStaticContext
      body.setRetainedStaticContext(rsc)
    }
    for (i <- 0 until function.getArity) {
      localBindings.pop
    }
    if (function.getDeclaredStreamability != FunctionStreamability.UNCLASSIFIED) addCompletionAction(() => function.prepareForStreaming)
    function
  }

  def makeFunction(flags: String) = new UserFunction

  @throws[XPathException]
  private def readAttributeSet(aSetElement: NodeInfo) = {
    val pack = packStack.peek
    localBindings = new Stack[LocalBinding]
    val aSetName = getQNameAttribute(aSetElement, "name")
    val slots = getIntegerAttribute(aSetElement, "slots")
    //System.err.println("Loading attribute set " + aSetName);
    val aSet = new AttributeSet
    aSet.setName(aSetName)
    aSet.setStackFrameMap(new SlotManager(slots))
    aSet.setPackageData(pack)
    aSet.setBody(getFirstChildExpression(aSetElement))
    aSet.setDeclaredStreamable("s" == aSetElement.getAttributeValue("", "flags"))
    aSet
  }

  @throws[XPathException]
  private def readMode(modeElement: NodeInfo): Actor = {
    val pack = packStack.peek
    var modeName = getQNameAttribute(modeElement, "name")
    if (modeName == null) modeName = Mode.UNNAMED_MODE_NAME
    //val mode = pack.getRuleManager.obtainMode(modeName, true).asInstanceOf[SimpleMode]
    val patternSlots = getIntegerAttribute(modeElement, "patternSlots")
    //mode.allocatePatternSlots(patternSlots)
    val onNoMatch = modeElement.getAttributeValue("", "onNo")
    var base = null
    /* if (onNoMatch != null) {
       base = mode.getBuiltInRuleSetForCode(onNoMatch)
       mode.setBuiltInRuleSet(base)
     }*/
    val flags = modeElement.getAttributeValue("", "flags")
    /*  if (flags != null) {
        mode.setStreamable(flags.contains("s"))
        if (flags.contains("t")) mode.setExplicitProperty("typed", "yes", 1)
        if (flags.contains("u")) mode.setExplicitProperty("typed", "no", 1)
        if (flags.contains("F")) mode.setRecoveryPolicy(RecoveryPolicy.DO_NOT_RECOVER)
        if (flags.contains("W")) mode.setRecoveryPolicy(RecoveryPolicy.RECOVER_WITH_WARNINGS)
        if (flags.contains("e")) mode.setHasRules(false)
      }*/
    val accNames = getListOfQNameAttribute(modeElement, "useAcc")
    addCompletionAction(() => {
      def foo() = {
        val registry = pack.getAccumulatorRegistry
        val accumulators = new HashSet[Accumulator]

        for (qn <- accNames.asScala) {
          val acc = registry.getAccumulator(qn)
          accumulators.add(acc)
        }
        //mode.setAccumulators(accumulators)
      }

      foo()
    })
    val iterator2 = modeElement.iterateAxis(AxisInfo.DESCENDANT, new NameTest(Type.ELEMENT,
      NamespaceConstant.SAXON_XSLT_EXPORT, "templateRule", config.getNamePool))
    var templateRuleElement0: NodeInfo = null
    val ruleStack = new LinkedList[NodeInfo]
    while ( {
      (templateRuleElement0 = iterator2.next) != null
    }) { // process rules in reverse order
      ruleStack.addFirst(templateRuleElement0)
    }

    for (templateRuleElement <- ruleStack.asScala) {
      val precedence = getIntegerAttribute(templateRuleElement, "prec")
      val rank = getIntegerAttribute(templateRuleElement, "rank")
      val priorityAtt = templateRuleElement.getAttributeValue("", "prio")
      val priority = priorityAtt.toDouble
      val sequence = getIntegerAttribute(templateRuleElement, "seq")
      var part = getIntegerAttribute(templateRuleElement, "part")
      if (part == Integer.MIN_VALUE) part = 0
      val minImportPrecedence = getIntegerAttribute(templateRuleElement, "minImp")
      val slots = getIntegerAttribute(templateRuleElement, "slots")
      val streamable = "1" == templateRuleElement.getAttributeValue("", "streamable")
      val tflags = templateRuleElement.getAttributeValue("", "flags")
      val contextType = parseAlphaCode(templateRuleElement, "cxt")
      val contextItemType = if (contextType == null) AnyItemType.getInstance
      else contextType.getPrimaryType
      val matchElement = getChildWithRole(templateRuleElement, "match")
      val `match` = loadPattern(matchElement)
      localBindings = new Stack[LocalBinding]
      val template = config.makeTemplateRule
      template.setMatchPattern(`match`)
      template.setStackFrameMap(new SlotManager(slots))
      template.setPackageData(pack)
      template.setRequiredType(parseAlphaCode(templateRuleElement, "as"))
      template.setDeclaredStreamable(streamable)
      template.setContextItemRequirements(contextItemType, !tflags.contains("s"))
      val bodyElement = getChildWithRole(templateRuleElement, "action")
      if (bodyElement == null) template.setBody(Literal.makeEmptySequence)
      else {
        val body = loadExpression(bodyElement)
        template.setBody(body)
        val rsc = body.getRetainedStaticContext
        body.setRetainedStaticContext(rsc)
      }
      /* val rule = mode.makeRule(`match`, template, precedence, minImportPrecedence, priority, sequence, part)
       rule.setRank(rank)
       mode.addRule(`match`, rule)
       mode.setHasRules(true)*/
    }
    /*  addCompletionAction(mode.prepareStreamability)*/
    null
  }

  @throws[XPathException]
  private def readAccumulators(packageElement: NodeInfo) = {
    val pack = packStack.peek
    var accElement: NodeInfo = null
    val iterator = packageElement.iterateAxis(AxisInfo.CHILD, new NameTest(Type.ELEMENT, NamespaceConstant.SAXON_XSLT_EXPORT, "accumulator", config.getNamePool))
    while ( {
      (accElement = iterator.next) != null
    }) {
      val accName = getQNameAttribute(accElement, "name")
      val acc = new Accumulator
      val component = Component.makeComponent(acc, Visibility.PRIVATE, VisibilityProvenance.DEFAULTED, pack, pack)
      acc.setDeclaringComponent(component)
      val iniSlots = getIntegerAttribute(accElement, "slots")
      acc.setSlotManagerForInitialValueExpression(new SlotManager(iniSlots))
      acc.setAccumulatorName(accName)
      val binds = accElement.getAttributeValue("", "binds")
      externalReferences.put(component, binds)
      val streamable = "1" == accElement.getAttributeValue("", "streamable")
      val flags = accElement.getAttributeValue("", "flags")
      val universal = flags != null && flags.contains("u")
      acc.setDeclaredStreamable(streamable)
      acc.setUniversallyApplicable(universal)
      val init = getExpressionWithRole(accElement, "init")
      acc.setInitialValueExpression(init)
      val pre = getChild(accElement, 1)
      readAccumulatorRules(acc, pre)
      val post = getChild(accElement, 2)
      readAccumulatorRules(acc, post)
      pack.getAccumulatorRegistry.addAccumulator(acc)
    }
  }

  @throws[XPathException]
  private def readAccumulatorRules(acc: Accumulator, owner: NodeInfo): Unit = {
    val iterator = owner.iterateAxis(AxisInfo.CHILD, new NameTest(Type.ELEMENT, NamespaceConstant.SAXON_XSLT_EXPORT, "accRule", config.getNamePool))
    var accRuleElement: NodeInfo = null
    val preDescent = owner.getLocalPart == "pre"
    val mode = if (preDescent) acc.getPreDescentRules
    else acc.getPostDescentRules
    val patternSlots = getIntegerAttribute(owner, "slots")
    mode.setStackFrameSlotsNeeded(patternSlots)
    while ( {
      (accRuleElement = iterator.next) != null
    }) {
      val slots = getIntegerAttribute(accRuleElement, "slots")
      val rank = getIntegerAttribute(accRuleElement, "rank")
      val flags = accRuleElement.getAttributeValue("", "flags")
      val sm = new SlotManager(slots)
      val pattern = getFirstChildPattern(accRuleElement)
      val select = getSecondChildExpression(accRuleElement)
      val rule = new AccumulatorRule(select, sm, !preDescent)
      if (flags != null && flags.contains("c")) rule.setCapturing(true)
      mode.addRule(pattern, mode.makeRule(pattern, rule, rank, 0, rank, 0, 0))
    }
    mode.computeRankings(1)
  }

  private def readOutputProperties(packageElement: NodeInfo) = {
    val pack = packStack.peek
    var outputElement: NodeInfo = null
    val iterator = packageElement.iterateAxis(AxisInfo.CHILD, new NameTest(Type.ELEMENT, NamespaceConstant.SAXON_XSLT_EXPORT, "output", config.getNamePool))
    while ( {
      (outputElement = iterator.next) != null
    }) {
      val outputName = getQNameAttribute(outputElement, "name")
      val props = new Properties
      var propertyElement: NodeInfo = null
      val iterator1 = outputElement.iterateAxis(AxisInfo.CHILD, new NameTest(Type.ELEMENT, NamespaceConstant.SAXON_XSLT_EXPORT, "property", config.getNamePool))
      while ( {
        (propertyElement = iterator1.next) != null
      }) {
        var name = propertyElement.getAttributeValue("", "name")
        if (name.startsWith("Q{")) name = name.substring(1)
        val value = propertyElement.getAttributeValue("", "value")
        if (name.startsWith("{http://saxon.sf.net/}") && !(name == SaxonOutputKeys.STYLESHEET_VERSION)) needsPELicense("Saxon output properties")
        props.setProperty(name, value)
      }
      if (outputName == null) pack.setDefaultOutputProperties(props)
      else pack.setNamedOutputProperties(outputName, props)
    }
  }

  @throws[XPathException]
  private def readCharacterMaps(packageElement: NodeInfo): Unit = {
    val pack = packStack.peek
    var charMapElement: NodeInfo = null
    val iterator = packageElement.iterateAxis(AxisInfo.CHILD, new NameTest(Type.ELEMENT, NamespaceConstant.SAXON_XSLT_EXPORT, "charMap", config.getNamePool))
    while ( {
      (charMapElement = iterator.next) != null
    }) {
      val mapName = getQNameAttribute(charMapElement, "name")
      var mappingElement: NodeInfo = null
      val iterator1 = charMapElement.iterateAxis(AxisInfo.CHILD, new NameTest(Type.ELEMENT, NamespaceConstant.SAXON_XSLT_EXPORT, "m", config.getNamePool))
      val map = new IntHashMap[String]
      while ( {
        (mappingElement = iterator1.next) != null
      }) {
        val c = getIntegerAttribute(mappingElement, "c")
        val s = mappingElement.getAttributeValue("", "s")
        map.put(c, s)
      }
      val characterMap = new CharacterMap(mapName, map)
      pack.getCharacterMapIndex.putCharacterMap(mapName, characterMap)
    }
  }

  @throws[XPathException]
  private def readSpaceStrippingRules(packageElement: NodeInfo): Unit = {
    val pack = packStack.peek
    var element: NodeInfo = null
    val iterator = packageElement.iterateAxis(AxisInfo.CHILD, NodeKindTest.ELEMENT)
    while ( {
      (element = iterator.next) != null
    }) {
      val s = element.getLocalPart
      s match {
        case "strip.all" =>
          pack.setStripperRules(new AllElementsSpaceStrippingRule)
          pack.setStripsWhitespace(true)
        case "strip.none" =>
          pack.setStripperRules(new NoElementsSpaceStrippingRule)
        case "strip" =>
          val iterator2 = element.iterateAxis(AxisInfo.CHILD, NodeKindTest.ELEMENT)
          var element2: NodeInfo = null
          val rules = new SelectedElementsSpaceStrippingRule(false)
          while ( {
            (element2 = iterator2.next) != null
          }) {
            val which = if (element2.getLocalPart == "s") Stripper.STRIP
            else Stripper.PRESERVE
            val value = element2.getAttributeValue("", "test")
            var t: NodeTest = null
            if (value == "*") t = NodeKindTest.ELEMENT
            else { // See bug 4096: this is not a true item type, it also allows *:name and name:*
              t = parseAlphaCodeForItemType(element2, "test").asInstanceOf[NodeTest]
            }
            val prec = getIntegerAttribute(element2, "prec")
            val pat = new NodeTestPattern(t)
            rules.addRule(pat, which, prec, prec)
          }
          pack.setStripperRules(rules)
          pack.setStripsWhitespace(true)
      }
    }
  }

  @throws[XPathException]
  private def readDecimalFormats(packageElement: NodeInfo): Unit = {
    var formatElement: NodeInfo = null
    val decimalFormatManager = packStack.peek.getDecimalFormatManager
    val iterator = packageElement.iterateAxis(AxisInfo.CHILD, new NameTest(Type.ELEMENT, NamespaceConstant.SAXON_XSLT_EXPORT, "decimalFormat", config.getNamePool))
    val propertyNames = DecimalSymbols.propertyNames
    while ( {
      (formatElement = iterator.next) != null
    }) {
      val name = getQNameAttribute(formatElement, "name")
      var symbols: DecimalSymbols = null
      if (name == null) symbols = decimalFormatManager.getDefaultDecimalFormat
      else symbols = decimalFormatManager.obtainNamedDecimalFormat(name)
      symbols.setHostLanguage(HostLanguage.XSLT, 31)
      for (p <- propertyNames) {
        if (formatElement.getAttributeValue("", p) != null) p match {
          case "NaN" =>
            symbols.setNaN(formatElement.getAttributeValue("", "NaN"))
          case "infinity" =>
            symbols.setInfinity(formatElement.getAttributeValue("", "infinity"))
          case "name" =>
          // no action
          case _ =>
            symbols.setIntProperty(p, getIntegerAttribute(formatElement, p))
        }
      }
    }
  }

  /**
   * Get the n'th element child of an element (zero-based)
   *
   * @param parent the parent element
   * @param n      which child to get (zero-based)
   * @return the n'th child, or null if not available
   */
  def getChild(parent: NodeInfo, n: Int): NodeInfo = {
    val iter = parent.iterateAxis(AxisInfo.CHILD, NodeKindTest.ELEMENT)
    var node = iter.next
    for (i <- 0 until n) {
      node = iter.next
    }
    node
  }

  def getChildWithRole(parent: NodeInfo, role: String): NodeInfo = {
    val iter = parent.iterateAxis(AxisInfo.CHILD, NodeKindTest.ELEMENT)
    var node: NodeInfo = null
    while ( {
      (node = iter.next) != null
    }) {
      val roleAtt = node.getAttributeValue("", "role")
      if (role == roleAtt) return node
    }
    null
  }

  @throws[XPathException]
  def getFirstChildExpression(parent: NodeInfo): Expression = {
    val node = parent.iterateAxis(AxisInfo.CHILD, NodeKindTest.ELEMENT).next
    loadExpression(node)
  }

  @throws[XPathException]
  def getSecondChildExpression(parent: NodeInfo): Expression = {
    val node = getChild(parent, 1)
    loadExpression(node)
  }

  @throws[XPathException]
  def getNthChildExpression(parent: NodeInfo, n: Int): Expression = {
    val node = getChild(parent, n)
    loadExpression(node)
  }

  @throws[XPathException]
  def getExpressionWithRole(parent: NodeInfo, role: String): Expression = {
    val node = getChildWithRole(parent, role)
    if (node == null) null
    else loadExpression(node)
  }

  @throws[XPathException]
  def loadExpression(element: NodeInfo): Expression = {
    if (element == null) return null
    val tag = element.getLocalPart
    val loader = PackageLoaderHE.eMap.get(tag)
    if (loader == null) {
      var message = "Cannot load expression with tag " + tag
      val req = PackageLoaderHE.licensableConstructs.get(tag)
      if (req != null) message += ". The stylesheet uses Saxon-" + req + " features"
      throw new XPathException(message, SaxonErrorCode.SXPK0002)
    }
    else {
      val rsc = makeRetainedStaticContext(element)
      contextStack.push(rsc)
      val exp = loader.loadFrom(this, element)
      exp.setRetainedStaticContextLocally(rsc)
      contextStack.pop
      exp.setLocation(makeLocation(element))
      return exp
    }
    null
  }

  private def makeLocation(element: NodeInfo) = {
    val lineAtt = getInheritedAttribute(element, "line")
    val moduleAtt = getInheritedAttribute(element, "module")
    if (lineAtt != null && moduleAtt != null) {
      val line = lineAtt.toInt
      allocateLocation(moduleAtt, line)
    }
    else Loc.NONE
  }

  def makeRetainedStaticContext(element: NodeInfo) = {
    val pack = packStack.peek
    val baseURIAtt = element.getAttributeValue("", "baseUri")
    val defaultCollAtt = element.getAttributeValue("", "defaultCollation")
    var defaultElementNS = element.getAttributeValue("", "defaultElementNS")
    var nsAtt = element.getAttributeValue("", "ns")
    val versionAtt = element.getAttributeValue("", "vn")
    if (baseURIAtt != null || defaultCollAtt != null || nsAtt != null || versionAtt != null || defaultElementNS != null || contextStack.peek.getDecimalFormatManager == null) {
      val rsc: RetainedStaticContext = new RetainedStaticContext(config)
      rsc.setPackageData(pack)
      if (defaultCollAtt != null) rsc.setDefaultCollationName(defaultCollAtt)
      else rsc.setDefaultCollationName(NamespaceConstant.CODEPOINT_COLLATION_URI)
      if (baseURIAtt != null) rsc.setStaticBaseUriString(baseURIAtt)
      else if (relocatableBase != null) rsc.setStaticBaseUriString(relocatableBase)
      else {
        val base = Navigator.getInheritedAttributeValue(element, "", "baseUri")
        if (base != null) rsc.setStaticBaseUriString(base)
      }
      if (nsAtt == null) nsAtt = Navigator.getInheritedAttributeValue(element, "", "ns")
      if (nsAtt != null && !nsAtt.isEmpty) {
        val namespaces = nsAtt.split(" ")
        for (ns <- namespaces) {
          val eq = ns.indexOf('=')
          if (eq < 0) throw new IllegalStateException("ns=" + nsAtt)
          val prefix = ns.substring(0, eq)
          var uri = ns.substring(eq + 1)
          if (uri == "~") uri = NamespaceConstant.getUriForConventionalPrefix(prefix)
          rsc.declareNamespace(prefix, uri)
        }
      }
      if (defaultElementNS == null) defaultElementNS = Navigator.getInheritedAttributeValue(element, "", "defaultElementNS")
      if (defaultElementNS != null) rsc.setDefaultElementNamespace(defaultElementNS)
      rsc.setDecimalFormatManager(packStack.peek.getDecimalFormatManager)
      rsc
    }

    else {
      contextStack.peek
    }
  }

  @throws[XPathException]
  private def getFirstChildPattern(parent: NodeInfo): Pattern = {
    val node: NodeInfo = parent.iterateAxis(AxisInfo.CHILD, NodeKindTest.ELEMENT).next
    loadPattern(node)
  }

  @throws[XPathException]
  private def getSecondChildPattern(parent: NodeInfo): Pattern = {
    val node: NodeInfo = getChild(parent, 1)
    loadPattern(node)
  }

  @throws[XPathException]
  def getPatternWithRole(parent: NodeInfo, role: String): Pattern = {
    val node: NodeInfo = getChildWithRole(parent, role)
    if (node == null) {
      null
    }
    else {
      loadPattern(node)
    }
  }

  @throws[XPathException]
  private def loadPattern(element: NodeInfo): Pattern = {
    val tag: String = element.getLocalPart
    val loader: PackageLoaderHE.PatternLoader = PackageLoaderHE.pMap.get(tag)
    if (loader == null) { //System.err.println("Cannot load pattern with tag " + tag);
      throw new XPathException("Cannot load pattern with tag " + tag, SaxonErrorCode.SXPK0002)
    }
    else {
      val pat: Pattern = loader.loadFrom(this, element)
      pat.setLocation(makeLocation(element))
      pat.setRetainedStaticContext(makeRetainedStaticContext(element))
      pat
    }
  }

  def getTypeAttribute(element: NodeInfo, attName: String): SchemaType = {
    val `val`: String = element.getAttributeValue("", attName)
    if (`val` == null) {
      return null
    }
    if (`val`.startsWith("xs:")) {
      config.getSchemaType(new StructuredQName("xs", NamespaceConstant.SCHEMA, `val`.substring(3)))
    }
    else {
      val name: StructuredQName = getQNameAttribute(element, attName)
      config.getSchemaType(name)
    }
  }

  def getQNameAttribute(element: NodeInfo, localName: String): StructuredQName = {
    val `val`: String = element.getAttributeValue("", localName)
    if (`val` == null) {
      return null
    }
    StructuredQName.fromEQName(`val`)
    //        int openBrace = val.indexOf('{');
    //        if (openBrace >= 0) {
    //            String prefix = val.substring(0, openBrace);
    //            int closeBrace = val.indexOf('}', openBrace+1);
    //            String uri = val.substring(openBrace+1, closeBrace);
    //            if (uri.equals("~")) {
    //                uri = NamespaceConstant.getUriForConventionalPrefix(prefix);
    //            String local = val.substring(closeBrace+1);
    //            return new StructuredQName(prefix, uri, local);
    //        } else {
    //            return new StructuredQName("", "", val);
    //        }
    //        //return resolveQName(val, element);
  }

  @throws[XPathException]
  def getListOfQNameAttribute(element: NodeInfo, localName: String): List[StructuredQName] = {
    val `val`: String = element.getAttributeValue("", localName)
    if (`val` == null) {
      return Collections.emptyList[StructuredQName]
    }
    val result: List[StructuredQName] = new ArrayList[StructuredQName]
    for (s <- `val`.split(" ")) {
      val sq: StructuredQName = resolveQName(s, element)
      result.add(sq)
    }
    result
  }

  @throws[XPathException]
  private def resolveQName(`val`: String, element: NodeInfo): StructuredQName = {
    if (`val`.startsWith("Q{")) {
      return StructuredQName.fromEQName(`val`)
    }
    else {
      if (`val`.contains(":")) {
        return StructuredQName.fromLexicalQName(`val`, true, true, element.getAllNamespaces)
      }
      else {
        return new StructuredQName("", "", `val`)
      }
    }
  }

  /**
   * Read an integer-valued attribute
   *
   * @param element   the element on which the attribute appears
   * @param localName the name of the attribute
   * @return the integer value of the attribute if present and correct; or Integer.MIN_VALUE if absent
   * @throws XPathException if the attribute is present but not integer-valued.
   */
  @throws[XPathException]
  def getIntegerAttribute(element: NodeInfo, localName: String): Int = {
    val `val`: String = element.getAttributeValue("", localName)
    if (`val` == null) {
      return Integer.MIN_VALUE
    }
    try return `val`.toInt
    catch {
      case e: NumberFormatException =>
        throw new XPathException("Expected integer value for " + element.getDisplayName + "/" + localName + ", found '" + `val` + "'", SaxonErrorCode.SXPK0002)
    }
  }

  def getInheritedAttribute(element: NodeInfo, localName: String): String = {
    var elem = element
    while (elem != null) {
      val `val`: String = elem.getAttributeValue("", localName)
      if (`val` != null) {
        return `val`
      }
      elem = elem.getParent
    }
    null
  }

  /**
   * Parse the SequenceType whose value is held in the attribute named "name"
   *
   * @param element the element containing this attribute
   * @param name    the local name of the attribute
   * @return the SequenceType held in the content of the attribute, or "item()*" if the attribute is absent
   * @throws XPathException if the sequence type is invalid
   */
  @throws[XPathException]
  def parseSequenceType(element: NodeInfo, name: String): SequenceType = {
    val env: IndependentContext = makeStaticContext(element)
    val attValue: String = element.getAttributeValue("", name)
    if (attValue == null) {
      return SequenceType.ANY_SEQUENCE
    }
    else {
      return parser.parseExtendedSequenceType(attValue, env)
    }
  }

  /**
   * Parse the SequenceType whose value is held in the attribute named "name", as an alphacode
   *
   * @param element the element containing this attribute
   * @param name    the local name of the attribute
   * @return the SequenceType held in the content of the attribute, or "item()*" if the attribute is absent
   * @throws XPathException if the sequence type is invalid
   */
  @throws[XPathException]
  def parseAlphaCode(element: NodeInfo, name: String): SequenceType = {
    val attValue: String = element.getAttributeValue("", name)
    if (attValue == null) {
      return SequenceType.ANY_SEQUENCE
    }
    else {
      try return AlphaCode.toSequenceType(attValue, config)
      catch {
        case e@(_: IllegalArgumentException | _: IllegalStateException) =>
          throw new XPathException("Invalid alpha code " + element.getDisplayName + "/@" + name + "='" + attValue + "': " + e.getMessage)
      }
    }
  }

  @throws[XPathException]
  def parseAlphaCodeForItemType(element: NodeInfo, name: String): ItemType = {
    val attValue: String = element.getAttributeValue("", name)
    if (attValue == null) {
      return AnyItemType.getInstance
    }
    else {
      try return AlphaCode.toItemType(attValue, config)
      catch {
        case e@(_: IllegalArgumentException | _: IllegalStateException) =>
          throw new XPathException("Invalid alpha code " + element.getDisplayName + "/@" + name + "='" + attValue + "': " + e.getMessage)
      }
    }
  }

  private def makeStaticContext(element: NodeInfo): IndependentContext = {
    val pack: StylesheetPackage = packStack.peek
    val env: IndependentContext = new IndependentContext(config)
    val resolver: NamespaceResolver = element.getAllNamespaces
    env.setNamespaceResolver(resolver)
    env.setImportedSchemaNamespaces(pack.getSchemaNamespaces)
    env.getImportedSchemaNamespaces.asJava.add(NamespaceConstant.ANONYMOUS)
    parser.setQNameParser(parser.getQNameParser.withNamespaceResolver(resolver))
    env
  }

  /**
   * Parse the ItemType whose value is held in the attribute named "name"
   *
   * @param element the element containing this attribute
   * @param attName the local name of the attribute
   * @return the SequenceType held in the content of the attribute, or "item()" if the attribute is absent
   * @throws XPathException if the item type is invalid
   */
  @throws[XPathException]
  def parseItemTypeAttribute(element: NodeInfo, attName: String): ItemType = {
    val attValue: String = element.getAttributeValue("", attName)
    if (attValue == null) {
      return AnyItemType.getInstance
    }
    return parseItemType(element, attValue)
  }

  @throws[XPathException]
  private def parseItemType(element: NodeInfo, attValue: String): ItemType = {
    val env: IndependentContext = makeStaticContext(element)
    return parser.parseExtendedItemType(attValue, env)
  }

  @throws[XPathException]
  def makeAtomicComparer(name: String, element: NodeInfo): AtomicComparer = {
    if (name == "CCC") {
      return CodepointCollatingComparer.getInstance
    }
    else {
      if (name == "CAVC") {
        return ComparableAtomicValueComparer.getInstance
      }
      else {
        if (name.startsWith("GAC|")) {
          val collator: StringCollator = config.getCollation(name.substring(4))
          return new GenericAtomicComparer(collator, null)
        }
        else {
          if (name == "CalVC") {
            return new CalendarValueComparer(null)
          }
          else {
            if (name == "EQC") {
              return EqualityComparer.getInstance
            }
            else {
              if (name == "NC") {
                return NumericComparer.getInstance
              }
              else {
                if (name == "NC11") {
                  return NumericComparer11.getInstance
                }
                else {
                  if (name == "QUNC") {
                    return new UntypedNumericComparer
                  }
                  else {
                    if (name == "DblSC") {
                      return DoubleSortComparer.getInstance
                    }
                    else {
                      if (name == "DecSC") {
                        return DecimalSortComparer.getDecimalSortComparerInstance
                      }
                      else {
                        if (name.startsWith("CAC|")) {
                          val collator: StringCollator = config.getCollation(name.substring(4))
                          return new CollatingAtomicComparer(collator)
                        }
                        else {
                          if (name.startsWith("AtSC|")) {
                            val nextBar: Int = name.indexOf('|', 5)
                            val fps: String = name.substring(5, nextBar)
                            val fp: Int = fps.toInt
                            val collName: String = name.substring(nextBar + 1)
                            return AtomicSortComparer.makeSortComparer(config.getCollation(collName), fp, new EarlyEvaluationContext(config))
                          }
                          else {
                            if (name.startsWith("DESC|")) {
                              val base: AtomicComparer = makeAtomicComparer(name.substring(5), element)
                              return new DescendingComparer(base)
                            }
                            else {
                              if (name.startsWith("TEXT|")) {
                                val base: AtomicComparer = makeAtomicComparer(name.substring(5), element)
                                return new TextComparer(base)
                              }
                              else {
                                throw new XPathException("Unknown comparer " + name, SaxonErrorCode.SXPK0002)
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  /**
   * Load a set of sort key definitions
   *
   * @param element the sort element containing the sort key definitions
   * @return the list of sort key definitions
   */
  @throws[XPathException]
  private def loadSortKeyDefinitions(element: NodeInfo): SortKeyDefinitionList = {
    val skdl: List[SortKeyDefinition] = new ArrayList[SortKeyDefinition](4)
    var sortKeyElement: NodeInfo = null
    val iterator: AxisIterator = element.iterateAxis(AxisInfo.CHILD, new NameTest(Type.ELEMENT, NamespaceConstant.SAXON_XSLT_EXPORT,
      "sortKey", config.getNamePool))
    while ( {
      (sortKeyElement = iterator.next) != null
    }) {
      val skd: SortKeyDefinition = new SortKeyDefinition
      val compAtt: String = sortKeyElement.getAttributeValue("", "comp")
      if (compAtt != null) {
        val ac: AtomicComparer = makeAtomicComparer(compAtt, sortKeyElement)
        skd.setFinalComparator(ac)
      }
      skd.setSortKey(getExpressionWithRole(sortKeyElement, "select"), true)
      skd.setOrder(getExpressionWithRole(sortKeyElement, "order"))
      skd.setLanguage(getExpressionWithRole(sortKeyElement, "lang"))
      skd.setCollationNameExpression(getExpressionWithRole(sortKeyElement, "collation"))
      skd.setCaseOrder(getExpressionWithRole(sortKeyElement, "caseOrder"))
      skd.setStable(getExpressionWithRole(sortKeyElement, "stable"))
      skd.setDataTypeExpression(getExpressionWithRole(sortKeyElement, "dataType"))
      skdl.add(skd)
    }
    return new SortKeyDefinitionList(skdl.toArray(new Array[SortKeyDefinition](0)))
  }

  @throws[XPathException]
  private def loadWithParams(element: NodeInfo, parent: Expression, needTunnel: Boolean): Array[WithParam] = {
    val wps: List[WithParam] = new ArrayList[WithParam](4)
    var wpElement: NodeInfo = null
    val iterator: AxisIterator = element.iterateAxis(AxisInfo.CHILD, new NameTest(Type.ELEMENT, NamespaceConstant.SAXON_XSLT_EXPORT,
      "withParam", config.getNamePool))
    while ( {
      (wpElement = iterator.next) != null
    }) {
      val flags: String = wpElement.getAttributeValue("", "flags")
      val isTunnel: Boolean = flags != null && flags.contains("t")
      if (needTunnel == isTunnel) {
        val wp: WithParam = new WithParam
        wp.setVariableQName(getQNameAttribute(wpElement, "name"))
        wp.setSelectExpression(parent, getFirstChildExpression(wpElement))
        wp.setRequiredType(parseAlphaCode(wpElement, "as"))
        wp.setTypeChecked(flags != null && flags.contains("c"))
        wps.add(wp)
      }
    }
    return wps.toArray(new Array[WithParam](0))
  }

  private def importProperties(value: String): Properties = {
    try {
      val reader: StringReader = new StringReader(value)
      val props: Properties = new Properties
      val lnr: LineNumberReader = new LineNumberReader(reader)
      var line: String = null
      while ( {
        (line = lnr.readLine) != null
      }) {
        val eq: Int = line.indexOf('=')
        var key: String = line.substring(0, eq)
        var `val`: String = if (eq == line.length - 1) {
          ""
        }
        else {
          line.substring(eq + 1)
        }
        if (key == "item-separator" || key == "Q" + SaxonOutputKeys.NEWLINE) {
          try `val` = JsonParser.unescape(`val`, 0, "", -(1))
          catch {
            case ignored: XPathException =>

            // No action, leave unescaped
          }
        }
        if (key.startsWith("Q{")) {
          key = key.substring(1)
        }
        props.setProperty(key, `val`)
      }
      return props
    } catch {
      case e: IOException =>
        throw new AssertionError(e)
    }
  }

  @throws[XPathException]
  private def resolveFixups(): Unit = {
    val pack: StylesheetPackage = packStack.peek
    breakable {
      for (call <- fixups.peek.asScala) {
        if (processComponentReference(pack, call)) {
          break
        }
      }
    }
    pack.allocateBinderySlots()
  }

  @throws[XPathException]
  def processComponentReference(pack: StylesheetPackage, call: ComponentInvocation): Boolean = {
    val sn: SymbolicName = call.getSymbolicName
    val c: Component = pack.getComponent(sn)
    if (c == null) {
      if (sn.getComponentName.hasURI(NamespaceConstant.XSLT) && sn.getComponentName.getLocalPart == "original") {
        return true
      }
      else {
        throw new XPathException("Loading compiled package: unresolved component reference to " + sn)
      }
    }
    if (call.isInstanceOf[GlobalVariableReference]) {
      (call.asInstanceOf[GlobalVariableReference]).setTarget(c)
    }
    else {
      if (call.isInstanceOf[UserFunctionCall]) {
        (call.asInstanceOf[UserFunctionCall]).setFunction(c.getActor.asInstanceOf[UserFunction])
        (call.asInstanceOf[UserFunctionCall]).setStaticType((c.getActor.asInstanceOf[UserFunction]).getResultType)
        //            } else if (call instanceof UserFunctionReference) {
        //                ((UserFunctionReference) call).setFunction((UserFunction) c.getActor());
      }
      else {
        if (call.isInstanceOf[CallTemplate]) {
          (call.asInstanceOf[CallTemplate]).setTargetTemplate(c.getActor.asInstanceOf[NamedTemplate])
        }
        else {
          if (call.isInstanceOf[UseAttributeSet]) {
            (call.asInstanceOf[UseAttributeSet]).setTarget(c.getActor.asInstanceOf[AttributeSet])
          }
          else {
            if (call.isInstanceOf[ApplyTemplates]) {
              (call.asInstanceOf[ApplyTemplates]).setMode(c.getActor.asInstanceOf[SimpleMode])
            }
            else {
              throw new XPathException("Unknown component reference " + call.getClass)
            }
          }
        }
      }
    }
    false
  }

  private def allocateLocation(module: String, lineNumber: Int): Location = {
    var lineMap: IntHashMap[Location] = locationMap.get(module)
    if (lineMap == null) {
      lineMap = new IntHashMap[Location]
      locationMap.put(module, lineMap)
    }
    var loc: Location = lineMap.get(lineNumber)
    if (loc == null) {
      loc = new Loc(module, lineNumber, -(1))
      lineMap.put(lineNumber, loc)
    }
    loc
  }
}

// Copyright (c) 2018-2020 Saxonica Limited