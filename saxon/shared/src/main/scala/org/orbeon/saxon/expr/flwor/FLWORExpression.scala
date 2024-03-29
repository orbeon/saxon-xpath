package org.orbeon.saxon.expr.flwor

import org.orbeon.saxon.event.Outputter
import org.orbeon.saxon.expr.Expression._
import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.flwor.FLWORExpression._
import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.model.ItemType
import org.orbeon.saxon.om.SequenceIterator
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value.SequenceType

import java.util.{ArrayList, List}

//import scala.collection.compat._
import org.orbeon.saxon.expr.flwor.Clause.ClauseName._
import org.orbeon.saxon.query.QueryModule

import scala.jdk.CollectionConverters._
import scala.util.control.Breaks
import scala.util.control.Breaks._


object FLWORExpression {

  def isLoopingClause(c: Clause): Boolean =
    c.getClauseKey == Clause.ClauseName.FOR      ||
    c.getClauseKey == Clause.ClauseName.GROUP_BY ||
    c.getClauseKey == Clause.ClauseName.WINDOW

  private val SINGLE_RETURN: OperandRole =
    new OperandRole(0, OperandUsage.TRANSMISSION, SequenceType.ANY_SEQUENCE)

  private val REPEATED_RETURN: OperandRole = new OperandRole(
    OperandRole.HIGHER_ORDER,
    OperandUsage.TRANSMISSION,
    SequenceType.ANY_SEQUENCE
  )
}

class FLWORExpression extends Expression {

  var clauses: List[Clause] = _

  var returnClauseOp: Operand = _

  def init(clauses: List[Clause], returnClause: Expression): Unit = {
    this.clauses = clauses
    var looping: Boolean = false
    breakable {
      for (c <- clauses.asScala if isLoopingClause(c)) {
        looping = true
        break()
      }
    }
    this.returnClauseOp = new Operand(
      this,
      returnClause,
      if (looping) REPEATED_RETURN else SINGLE_RETURN
    )
  }

  def getClauseList: List[Clause] = clauses

  def getReturnClause: Expression = returnClauseOp.getChildExpression

  override def hasVariableBinding(binding: Binding): Boolean =
    clauses.asScala.exists(clauseHasBinding(_, binding))

  private def clauseHasBinding(c: Clause, binding: Binding): Boolean =
    c.getRangeVariables.exists(_ eq binding)

  override def allowExtractingCommonSubexpressions(): Boolean = false

  override def simplify(): Expression = {
    val simplifier: OperandProcessor = op => op.setChildExpression(op.getChildExpression.simplify())
    for (c <- clauses.asScala)
      c.processOperands(simplifier)
    returnClauseOp.setChildExpression(getReturnClause.simplify())
    this
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    val typeChecker: OperandProcessor = op =>
      op.typeCheck(visitor, contextInfo)
    for (i <- 0 until clauses.size) {
      clauses.get(i).processOperands(typeChecker)
      clauses.get(i).typeCheck(visitor, contextInfo)
      val bindings: Array[LocalVariableBinding] =
        clauses.get(i).getRangeVariables
      for (b <- bindings) {
        val references = new ArrayList[VariableReference]()
        for (j <- i until clauses.size)
          clauses.get(j).gatherVariableReferences(visitor, b, references)
        ExpressionTool.gatherVariableReferences(getReturnClause, b, references)
        clauses.get(i).refineVariableType(visitor, references, getReturnClause)
      }
    }
    returnClauseOp.typeCheck(visitor, contextInfo)
    this
  }

  override def implementsStaticTypeCheck(): Boolean = {
    for (c <- clauses.asScala)
      c.getClauseKey match {
        case LET | WHERE => //continue
        case _ => return false
      }
    true
  }

  override def staticTypeCheck(req: SequenceType,
                               backwardsCompatible: Boolean,
                               role: RoleDiagnostic,
                               visitor: ExpressionVisitor): Expression = {
    val tc = visitor.getConfiguration.getTypeChecker(backwardsCompatible)
    returnClauseOp.setChildExpression(tc.staticTypeCheck(getReturnClause, req, role, visitor))
    this
  }

  override def getItemType: ItemType = getReturnClause.getItemType

  override def computeCardinality(): Int =
    StaticProperty.ALLOWS_ZERO_OR_MORE

  override def operands: java.lang.Iterable[Operand] = {
    val list = new ArrayList[Operand](5)
    var repeatable = false
    for (c <- clauses.asScala) {
      c.processOperands(res => list.add(res))
      if (c.isInstanceOf[ForClause])
        repeatable = true
    }
    list.add(returnClauseOp)
    list
  }

  override def checkForUpdatingSubexpressions(): Unit = {
    val processor: OperandProcessor = op => {
      op.getChildExpression.checkForUpdatingSubexpressions()
      if (op.getChildExpression.isUpdatingExpression) {
        throw new XPathException(
          "An updating expression cannot be used in a clause of a FLWOR expression",
          "XUST0001")
      }
    }
    for (c <- clauses.asScala) {
      c.processOperands(processor)
    }
    getReturnClause.checkForUpdatingSubexpressions()
  }

  override def isUpdatingExpression: Boolean =
    getReturnClause.isUpdatingExpression

  override def getImplementationMethod: Int = ITERATE_METHOD | PROCESS_METHOD

  override def addToPathMap(
    pathMap        : PathMap,
    pathMapNodeSet : PathMap.PathMapNodeSet
  ): PathMap.PathMapNodeSet = {
    for (c <- clauses.asScala)
      c.addToPathMap(pathMap, pathMapNodeSet)
    getReturnClause.addToPathMap(pathMap, pathMapNodeSet)
  }

  def injectCode(injector: CodeInjector): Unit = {
    if (injector != null) {
      val expandedList = new ArrayList[Clause](clauses.size * 2)
      expandedList.add(clauses.get(0))
      for (i <- 1 until clauses.size) {
        val extra = injector.injectClause(this, clauses.get(i - 1))
        if (extra != null)
          expandedList.add(extra)
        expandedList.add(clauses.get(i))
      }
      val extra = injector.injectClause(this, clauses.get(clauses.size - 1))
      if (extra != null)
        expandedList.add(extra)
      clauses = expandedList
    }
  }

  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("FLWOR", this)
    for (c <- clauses.asScala)
      c.explain(out)
    out.startSubsidiaryElement("return")
    getReturnClause.export(out)
    out.endSubsidiaryElement()
    out.endElement()
  }

  override def copy(rebindings: RebindingMap): Expression = {
    val newClauses = new ArrayList[Clause]()
    val f2         = new FLWORExpression
    for (c <- clauses.asScala) {
      val c2 = c.copy(f2, rebindings)
      c2.setLocation(c.getLocation)
      c2.setRepeated(c.isRepeated)
      val oldBindings = c.getRangeVariables
      val newBindings = c2.getRangeVariables
      assert(oldBindings.length == newBindings.length)
      for (i <- oldBindings.indices)
        rebindings.put(oldBindings(i), newBindings(i))
      newClauses.add(c2)
    }
    f2.init(newClauses, getReturnClause.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, f2)
    f2
  }

  override def unordered(retainAllNodes: Boolean,
                         forStreaming: Boolean): Expression = {
    for (c <- clauses.asScala if c.isInstanceOf[ForClause] &&
      c.asInstanceOf[ForClause].getPositionVariable == null) {
      c.asInstanceOf[ForClause]
        .setSequence(
          c.asInstanceOf[ForClause]
            .getSequence
            .unordered(retainAllNodes, forStreaming))
    }
    returnClauseOp.setChildExpression(
      getReturnClause.unordered(retainAllNodes, forStreaming))
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    for (c <- clauses.asScala) {
      c.processOperands(op => op.optimize(visitor, contextItemType))
      c.optimize(visitor, contextItemType)
    }
    returnClauseOp.setChildExpression(
      getReturnClause.optimize(visitor, contextItemType))
    if (clauses.size == 1) {
      val c = clauses.get(0)
      if (c.isInstanceOf[LetClause] ||
        (c.isInstanceOf[ForClause] &&
          c.asInstanceOf[ForClause].getPositionVariable == null)) {
        return rewriteForOrLet(visitor, contextItemType)
      }
    }
    var tryAgain = false
    var changed  = false
    val outer = new Breaks
    val inner = new Breaks
    do {
      tryAgain = false
      outer.breakable {
        for (c <- clauses.asScala if c.getClauseKey == Clause.ClauseName.LET) {
          val lc = c.asInstanceOf[LetClause]
          if (!ExpressionTool.dependsOnVariable(this,
            Array(lc.getRangeVariable))) {
            clauses.remove(c)
            tryAgain = true
            outer.break()
          }
          var suppressInlining = false
          inner.breakable {
            for (c2 <- clauses.asScala if c2.containsNonInlineableVariableReference(
              lc.getRangeVariable)) {
              suppressInlining = true
              inner.break()
            }
          }
          if (!suppressInlining) {
            val oneRef    = lc.getRangeVariable.getNominalReferenceCount == 1
            val simpleSeq = lc.getSequence
              .isInstanceOf[VariableReference] || lc.getSequence
              .isInstanceOf[Literal]
            if (oneRef || simpleSeq) {
              ExpressionTool.replaceVariableReferences(this,
                lc.getRangeVariable,
                lc.getSequence,
                mustCopy = true
              )
              clauses.remove(c)
              if (clauses.isEmpty) {
                return getReturnClause
              }
              tryAgain = true
              outer.break()
            }
          }
        }
      }
      changed |= tryAgain
    } while (tryAgain)
    if (changed) {
      var i = clauses.size - 1
      while (i >= 1) {
        if (
          clauses.get(i).getClauseKey     == Clause.ClauseName.TRACE &&
          clauses.get(i - 1).getClauseKey == Clause.ClauseName.TRACE
        ) {
          clauses.remove(i)
        }
        i -= 1
      }
    }
    var depends = false
    breakable {
      for (w <- clauses.asScala if w.isInstanceOf[WhereClause] &&
        ExpressionTool.dependsOnFocus(
          w.asInstanceOf[WhereClause].getPredicate)) {
        depends = true
        break()
      }
    }
    if (depends && contextItemType != null) {
      val expr1 = ExpressionTool.tryToFactorOutDot(this, contextItemType.getItemType)
      if (expr1 == null || (expr1 eq this))
        return this
      resetLocalStaticProperties()
      expr1.optimize(visitor, contextItemType)
    }
    val expr2 = rewriteWhereClause(visitor, contextItemType)
    if (expr2 != null && (expr2 ne this))
      return expr2.optimize(visitor, contextItemType)
    var allForOrLetExpr = true
    breakable {
      for (c <- clauses.asScala) {
        c match {
          case clause: ForClause =>
            if (clause.getPositionVariable != null) {
              allForOrLetExpr = false
              break()
            }
          case _ => if (! c.isInstanceOf[LetClause]) {
            allForOrLetExpr = false
            break()
          }
        }
      }
    }
    if (allForOrLetExpr)
      rewriteForOrLet(visitor, contextItemType)
    else
      this
  }

  private def rewriteWhereClause(
                                  visitor: ExpressionVisitor,
                                  contextItemType: ContextItemStaticInfo): Expression = {
    var whereClause: WhereClause = null
    var whereIndex               = 0
    class WhereClauseStruct {
      var whereIndex               = 0
      var whereClause: WhereClause = _
    }
    val whereList = new ArrayList[WhereClauseStruct]()
    for (c <- clauses.asScala) {
      c match {
        case clause: WhereClause =>
          val wStruct: WhereClauseStruct = new WhereClauseStruct()
          wStruct.whereClause = clause
          wStruct.whereIndex = clauses.size - whereIndex
          whereList.add(wStruct)
        case _ =>
      }
      whereIndex += 1
    }
    if (whereList.size == 0)
      return null
    while (!whereList.isEmpty) {
      whereClause = whereList.get(0).whereClause
      whereIndex = whereList.get(0).whereIndex
      val condition = whereClause.getPredicate
      val list      = new ArrayList[Expression](5)
      BooleanExpression.listAndComponents(condition, list)
      var i = list.size - 1
      while (i >= 0) {
        val term = list.get(i)
        var c = clauses.size - whereIndex - 1
        breakable {
          while (c >= 0) {
            val clause      = clauses.get(c)
            val bindingList = clause.getRangeVariables.asInstanceOf[Array[Binding]]
            if (ExpressionTool.dependsOnVariable(term, bindingList) ||
              clause.getClauseKey == Clause.ClauseName.COUNT) {
              val removedExpr = list.remove(i)
              if (list.isEmpty)
                clauses.remove(clauses.size - whereIndex)
              else
                whereClause.setPredicate(makeAndCondition(list))
              clause match {
                case forClause: ForClause if ! forClause.isAllowingEmpty =>
                  val added = forClause.addPredicate(this, visitor, contextItemType, term)
                  if (!added)
                    clauses.add(c + 1, new WhereClause(this, removedExpr))
                case _ =>
                  val newWhere = new WhereClause(this, term)
                  clauses.add(c + 1, newWhere)
              }
              break()
            }
            c -= 1
          }
        }
        if (list.size - 1 == i) {
          list.remove(i)
          if (list.isEmpty)
            clauses.remove(clauses.size - whereIndex)
          else
            whereClause.setPredicate(makeAndCondition(list))
          val newWhere = new WhereClause(this, term)
          clauses.add(0, newWhere)
        }
        i -= 1
      }
      whereList.remove(0)
    }
    this
  }

  private def makeAndCondition(list: List[Expression]): Expression =
    if (list.size == 1)
      list.get(0)
    else
      new AndExpression(list.get(0), makeAndCondition(list.subList(1, list.size)))

  private def rewriteForOrLet(
                               visitor: ExpressionVisitor,
                               contextItemType: ContextItemStaticInfo): Expression = {
    var action                 = getReturnClause
    var injector: CodeInjector = null
    visitor.getStaticContext match {
      case module: QueryModule =>
        injector = module.getCodeInjector
      case _ =>
    }
    var i = clauses.size - 1
    while (i >= 0) {
      clauses.get(i) match {
        case forClause: ForClause =>
          var forExpr: ForExpression = null
          forExpr =
            if (forClause.isAllowingEmpty)
              new OuterForExpression
            else
              new ForExpression
          forExpr.setLocation(forClause.getLocation)
          forExpr.setRetainedStaticContext(getRetainedStaticContext)
          forExpr.setAction(action)
          forExpr.setSequence(forClause.getSequence)
          forExpr.setVariableQName(forClause.getRangeVariable.getVariableQName)
          forExpr.setRequiredType(forClause.getRangeVariable.getRequiredType)
          ExpressionTool.rebindVariableReferences(action,
            forClause.getRangeVariable,
            forExpr)
          action = forExpr
        case _ =>
          val letClause = clauses.get(i).asInstanceOf[LetClause]
          val letExpr   = new LetExpression
          letExpr.setLocation(letClause.getLocation)
          letExpr.setRetainedStaticContext(getRetainedStaticContext)
          letExpr.setAction(action)
          letExpr.setSequence(letClause.getSequence)
          letExpr.setVariableQName(letClause.getRangeVariable.getVariableQName)
          letExpr.setRequiredType(letClause.getRangeVariable.getRequiredType)
          if (letClause.getRangeVariable.isIndexedVariable)
            letExpr.setIndexedVariable()
          ExpressionTool.rebindVariableReferences(action,
            letClause.getRangeVariable,
            letExpr)
          action = letExpr
      }
      i -= 1
    }
    action = action.typeCheck(visitor, contextItemType)
    action = action.optimize(visitor, contextItemType)
    action
  }

  override def iterate(context: XPathContext): SequenceIterator = {
    var stream: TuplePull = new SingularityPull
    for (c <- clauses.asScala) {
      stream = c.getPullStream(stream, context)
    }
    new ReturnClauseIterator(stream, this, context)
  }

  override def process(output: Outputter, context: XPathContext): Unit = {
    var destination: TuplePush = new ReturnClausePush(output, getReturnClause)
    var i                      = clauses.size - 1
    while (i >= 0) {
      val c = clauses.get(i)
      destination = c.getPushStream(destination, output, context)
      i -= 1
    }
    destination.processTuple(context)
    destination.close()
  }

  override def evaluatePendingUpdates(context: XPathContext,
                                      pul: PendingUpdateList): Unit = {
    var stream: TuplePull = new SingularityPull
    for (c <- clauses.asScala) {
      stream = c.getPullStream(stream, context)
    }
    while (stream.nextTuple(context)) getReturnClause.evaluatePendingUpdates(
      context,
      pul)
  }

  override def getExpressionName: String = "FLWOR"

  override def toShortString: String = {
    val sb = new FastStringBuffer(FastStringBuffer.C64)
    sb.append(clauses.get(0).toShortString)
    sb.append(" ... return ")
    sb.append(getReturnClause.toShortString)
    sb.toString
  }

  override def toString: String = {
    val sb = new FastStringBuffer(FastStringBuffer.C64)
    for (c <- clauses.asScala) {
      sb.append(c.toString)
      sb.cat(' ')
    }
    sb.append(" return ")
    sb.append(getReturnClause.toString)
    sb.toString
  }

  def hasLoopingVariableReference(binding: Binding): Boolean = {
    var bindingClause = -1
    breakable {
      for (i <- 0 until clauses.size
           if clauseHasBinding(clauses.get(i), binding)) {
        bindingClause = i
        break()
      }
    }
    val boundOutside = bindingClause < 0
    if (boundOutside)
      bindingClause = 0
    var lastReferencingClause = clauses.size
    if (!ExpressionTool.dependsOnVariable(getReturnClause, Array(binding))) {
      val response                  = new ArrayList[Boolean]()
      val checker: OperandProcessor = op =>
        if (response.isEmpty &&
          ExpressionTool.dependsOnVariable(op.getChildExpression,
            Array(binding))) {
          response.add(true)
        }
      var i       = clauses.size - 1
      breakable {
        while (i >= 0) {
          try {
            clauses.get(i).processOperands(checker)
            if (!response.isEmpty) {
              lastReferencingClause = i
              break()
            }
          } catch {
            case _: XPathException =>
              assert(false)
          }
          i -= 1
        }
      }
    }
    var i = lastReferencingClause - 1
    while (i >= bindingClause) {
      if (isLoopingClause(clauses.get(i)))
        return true
      i -= 1
    }
    false
  }
}
