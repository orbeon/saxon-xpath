package net.sf.saxon.sxpath

import net.sf.saxon.expr.Expression
import net.sf.saxon.expr.PackageData
import net.sf.saxon.expr.instruct.Executable
import net.sf.saxon.expr.instruct.SlotManager
import net.sf.saxon.expr.parser._
import net.sf.saxon.functions.FunctionLibrary
import net.sf.saxon.functions.FunctionLibraryList
import net.sf.saxon.functions.registry.ConstructorFunctionLibrary
import net.sf.saxon.model.ItemType
import net.sf.saxon.model.Type
import net.sf.saxon.pattern.Pattern
import net.sf.saxon.s9api.HostLanguage
import net.sf.saxon.utils.Configuration

import scala.beans.{BeanProperty, BooleanBeanProperty}

class XPathEvaluator(config: Configuration) {

  @BeanProperty
  var staticContext: XPathStaticContext = new IndependentContext(config)

  def getConfiguration(): Configuration = staticContext.getConfiguration

  def createExpression(expression: String): XPathExpression = {
    val config: Configuration = getConfiguration
    val exec: Executable = new Executable(config)
    exec.setTopLevelPackage(staticContext.getPackageData)
    exec.setSchemaAware(staticContext.getPackageData.isSchemaAware)
    exec.setHostLanguage(HostLanguage.XPATH)
    val userlib: FunctionLibrary = exec.getFunctionLibrary
    val lib: FunctionLibraryList = new FunctionLibraryList()
    lib.addFunctionLibrary(config.getXPath31FunctionSet)
    lib.addFunctionLibrary(config.getBuiltInExtensionLibraryList)
    lib.addFunctionLibrary(new ConstructorFunctionLibrary(config))
    lib.addFunctionLibrary(config.getIntegratedFunctionLibrary)
    config.addExtensionBinders(lib)
    if (userlib != null) {
      lib.addFunctionLibrary(userlib)
    }
    exec.setFunctionLibrary(lib)
    val opt: Optimizer = config.obtainOptimizer
    var exp: Expression =
      ExpressionTool.make(expression, staticContext, 0, -1, null)
    val rsc: RetainedStaticContext = staticContext.makeRetainedStaticContext()
    exp.setRetainedStaticContext(rsc)
    val visitor: ExpressionVisitor = ExpressionVisitor.make(staticContext)
    val contextItemType: ItemType = staticContext.getRequiredContextItemType
    val cit: ContextItemStaticInfo =
      config.makeContextItemStaticInfo(contextItemType, true)
    cit.setParentless(staticContext.isContextItemParentless)
    exp = exp.typeCheck(visitor, cit)
    if (opt.isOptionSet(OptimizerOptions.MISCELLANEOUS)) {
      exp = exp.optimize(visitor, cit)
    }
    if (opt.isOptionSet(OptimizerOptions.LOOP_LIFTING)) {
      exp.setParentExpression(null)
      exp = LoopLifter.process(exp, visitor, cit)
    }
    exp = postProcess(exp, visitor, cit)
    exp.setRetainedStaticContext(rsc)
    val map: SlotManager = staticContext.getStackFrameMap
    val numberOfExternalVariables: Int = map.getNumberOfVariables
    ExpressionTool.allocateSlots(exp, numberOfExternalVariables, map)
    val xpe: XPathExpression = new XPathExpression(staticContext, exp, exec)
    xpe.setStackFrameMap(map, numberOfExternalVariables)
    xpe
  }

   def postProcess(exp: Expression,
                            visitor: ExpressionVisitor,
                            cit: ContextItemStaticInfo): Expression = exp

  def createPattern(pattern: String): XPathExpression = {
    val config: Configuration = getConfiguration
    val exec: Executable = new Executable(config)
    val pat: Pattern =
      Pattern.make(pattern, staticContext, new PackageData(config))
    val visitor: ExpressionVisitor = ExpressionVisitor.make(staticContext)
    pat.typeCheck(visitor,
      config.makeContextItemStaticInfo(Type.NODE_TYPE, true))
    val map: SlotManager = staticContext.getStackFrameMap
    var slots: Int = map.getNumberOfVariables
    slots = pat.allocateSlots(map, slots)
    val xpe: XPathExpression = new XPathExpression(staticContext, pat, exec)
    xpe.setStackFrameMap(map, slots)
    xpe
  }

}
