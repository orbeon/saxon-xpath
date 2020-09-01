package net.sf.saxon.sxpath

import net.sf.saxon.expr.{Expression, PackageData}
import net.sf.saxon.expr.instruct.{Executable, SlotManager}
import net.sf.saxon.expr.parser._
import net.sf.saxon.functions.FunctionLibraryList
import net.sf.saxon.functions.registry.ConstructorFunctionLibrary
import net.sf.saxon.model.Type
import net.sf.saxon.pattern.Pattern
import net.sf.saxon.s9api.HostLanguage
import net.sf.saxon.utils.Configuration

import scala.beans.BeanProperty

class XPathEvaluator(config: Configuration) {

  @BeanProperty
  var staticContext: XPathStaticContext = new IndependentContext(config)

  def getConfiguration: Configuration = staticContext.getConfiguration

  def createExpression(expression: String): XPathExpression = {
    val config  = getConfiguration
    val exec = new Executable(config)
    exec.setTopLevelPackage(staticContext.getPackageData)
    exec.setSchemaAware(staticContext.getPackageData.isSchemaAware)
    exec.setHostLanguage(HostLanguage.XPATH)
    val userlib = exec.getFunctionLibrary

    val lib = new FunctionLibraryList
    lib.addFunctionLibrary(config.getXPath31FunctionSet)
    lib.addFunctionLibrary(config.getBuiltInExtensionLibraryList)
    lib.addFunctionLibrary(new ConstructorFunctionLibrary(config))
    lib.addFunctionLibrary(config.getIntegratedFunctionLibrary)

    config.addExtensionBinders(lib)
    if (userlib != null)
      lib.addFunctionLibrary(userlib)

    exec.setFunctionLibrary(lib)

    val opt = config.obtainOptimizer
    var exp =
      ExpressionTool.make(expression, staticContext, 0, -1, null)

    val rsc = staticContext.makeRetainedStaticContext
    exp.setRetainedStaticContext(rsc)

    val visitor = ExpressionVisitor.make(staticContext)
    val contextItemType = staticContext.getRequiredContextItemType

    val cit =
      config.makeContextItemStaticInfo(contextItemType, maybeUndefined = true)

    cit.setParentless(staticContext.isContextItemParentless)

    exp = exp.typeCheck(visitor, cit)
    if (opt.isOptionSet(OptimizerOptions.MISCELLANEOUS))
      exp = exp.optimize(visitor, cit)

    if (opt.isOptionSet(OptimizerOptions.LOOP_LIFTING)) {
      exp.setParentExpression(null)
      exp = LoopLifter.process(exp, visitor, cit)
    }

    exp = postProcess(exp, visitor, cit)
    exp.setRetainedStaticContext(rsc)
    val map = staticContext.getStackFrameMap
    val numberOfExternalVariables = map.getNumberOfVariables

    ExpressionTool.allocateSlots(exp, numberOfExternalVariables, map)

    val xpe = new XPathExpression(staticContext, exp, exec)
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
      config.makeContextItemStaticInfo(Type.NODE_TYPE, maybeUndefined = true))
    val map: SlotManager = staticContext.getStackFrameMap
    var slots: Int = map.getNumberOfVariables
    slots = pat.allocateSlots(map, slots)
    val xpe: XPathExpression = new XPathExpression(staticContext, pat, exec)
    xpe.setStackFrameMap(map, slots)
    xpe
  }

}
