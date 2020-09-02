package net.sf.saxon.expr.flwor

import net.sf.saxon.event.Outputter
import net.sf.saxon.expr._
import net.sf.saxon.expr.parser._
import net.sf.saxon.model.ItemType
import net.sf.saxon.om.SequenceIterator
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.util.FastStringBuffer
import net.sf.saxon.value.SequenceType
import java.util.ArrayList
import java.util.List

import FLWORExpression._
import Expression._

import scala.jdk.CollectionConverters._
import Clause.ClauseName._
import net.sf.saxon.query.QueryModule
import scala.util.control.Breaks
import scala.util.control.Breaks._

object FLWORExpression {

  def isLoopingClause(c: Clause): Boolean =
    c.getClauseKey == Clause.ClauseName.FOR || c.getClauseKey == Clause.ClauseName.GROUP_BY ||
      c.getClauseKey == Clause.ClauseName.WINDOW

  private val SINGLE_RETURN: OperandRole =
    new OperandRole(0, OperandUsage.TRANSMISSION, SequenceType.ANY_SEQUENCE)

  private val REPEATED_RETURN: OperandRole = new OperandRole(
    OperandRole.HIGHER_ORDER,
    OperandUsage.TRANSMISSION,
    SequenceType.ANY_SEQUENCE)

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
      if (looping) REPEATED_RETURN else SINGLE_RETURN)
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
    val tc: TypeChecker = visitor.getConfiguration.getTypeChecker(backwardsCompatible)
    returnClauseOp.setChildExpression(tc.staticTypeCheck(getReturnClause, req, role, visitor))
    this
  }

  override def getItemType: ItemType = getReturnClause.getItemType

  override def computeCardinality(): Int =
    StaticProperty.ALLOWS_ZERO_OR_MORE

  override def operands: java.lang.Iterable[Operand] = {
    val list: List[Operand] = new ArrayList[Operand](5)
    var repeatable: Boolean = false
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

  override def isUpdatingExpression(): Boolean =
    getReturnClause.isUpdatingExpression

  override def getImplementationMethod: Int = ITERATE_METHOD | PROCESS_METHOD

  override def addToPathMap(
                             pathMap: PathMap,
                             pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet = {
    for (c <- clauses.asScala) {
      c.addToPathMap(pathMap, pathMapNodeSet)
    }
    getReturnClause.addToPathMap(pathMap, pathMapNodeSet)
  }

  def injectCode(injector: CodeInjector): Unit = {
    if (injector != null) {
      val expandedList: List[Clause] = new ArrayList[Clause](clauses.size * 2)
      expandedList.add(clauses.get(0))
      for (i <- 1 until clauses.size) {
        val extra: Clause = injector.injectClause(this, clauses.get(i - 1))
        if (extra != null) {
          expandedList.add(extra)
        }
        expandedList.add(clauses.get(i))
      }
      val extra: Clause =
        injector.injectClause(this, clauses.get(clauses.size - 1))
      if (extra != null) {
        expandedList.add(extra)
      }
      clauses = expandedList
    }
  }

  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("FLWOR", this)
    for (c <- clauses.asScala) {
      c.explain(out)
    }
    out.startSubsidiaryElement("return")
    getReturnClause.export(out)
    out.endSubsidiaryElement()
    out.endElement()
  }

  override def copy(rebindings: RebindingMap): Expression = {
    val newClauses: List[Clause] = new ArrayList[Clause]()
    val f2: FLWORExpression = new FLWORExpression()
    for (c <- clauses.asScala) {
      val c2: Clause = c.copy(f2, rebindings)
      c2.setLocation(c.getLocation)
      c2.setRepeated(c.isRepeated)
      val oldBindings: Array[LocalVariableBinding] = c.getRangeVariables
      val newBindings: Array[LocalVariableBinding] = c2.getRangeVariables
      assert(oldBindings.length == newBindings.length)
      for (i <- 0 until oldBindings.length) {
        rebindings.put(oldBindings(i), newBindings(i))
      }
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
      val c: Clause = clauses.get(0)
      if (c.isInstanceOf[LetClause] ||
        (c.isInstanceOf[ForClause] &&
          c.asInstanceOf[ForClause].getPositionVariable == null)) {
        rewriteForOrLet(visitor, contextItemType)
      }
    }
    var tryAgain: Boolean = false
    var changed: Boolean = false
    val outer = new Breaks
    val inner = new Breaks
    do {
      tryAgain = false
      outer.breakable {
        for (c <- clauses.asScala if c.getClauseKey == Clause.ClauseName.LET) {
          val lc: LetClause = c.asInstanceOf[LetClause]
          if (!ExpressionTool.dependsOnVariable(this,
            Array(lc.getRangeVariable))) {
            clauses.remove(c)
            tryAgain = true
            outer.break()
          }
          var suppressInlining: Boolean = false
          inner.breakable {
            for (c2 <- clauses.asScala if c2.containsNonInlineableVariableReference(
              lc.getRangeVariable)) {
              suppressInlining = true
              inner.break()
            }
          }
          if (!suppressInlining) {
            val oneRef: Boolean = lc.getRangeVariable.getNominalReferenceCount == 1
            val simpleSeq: Boolean = lc.getSequence
              .isInstanceOf[VariableReference] || lc.getSequence
              .isInstanceOf[Literal]
            if (oneRef || simpleSeq) {
              ExpressionTool.replaceVariableReferences(this,
                lc.getRangeVariable,
                lc.getSequence,
                !oneRef)
              clauses.remove(c)
              if (clauses.isEmpty) {
                getReturnClause
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
      var i: Int = clauses.size - 1
      while (i >= 1) {
        if (clauses.get(i).getClauseKey == Clause.ClauseName.TRACE &&
          clauses.get(i - 1).getClauseKey == Clause.ClauseName.TRACE) {
          clauses.remove(i)
        }
        {
          i -= 1
          i + 1
        }
      }
    }
    var depends: Boolean = false
    breakable {
      for (w <- clauses.asScala if w.isInstanceOf[WhereClause] &&
        ExpressionTool.dependsOnFocus(
          w.asInstanceOf[WhereClause].getPredicate)) {
        depends = true
        break()
      }
    }
    if (depends && contextItemType != null) {
      val expr1: Expression =
        ExpressionTool.tryToFactorOutDot(this, contextItemType.getItemType)
      if (expr1 == null || expr1 == this) {
        return this
      }
      resetLocalStaticProperties()
      expr1.optimize(visitor, contextItemType)
    }
    val expr2: Expression = rewriteWhereClause(visitor, contextItemType)
    if (expr2 != null && expr2 != this) {
      expr2.optimize(visitor, contextItemType)
    }
    var allForOrLetExpr: Boolean = true
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
    if (allForOrLetExpr) {
      rewriteForOrLet(visitor, contextItemType)
    }
    this
  }

  private def rewriteWhereClause(
                                  visitor: ExpressionVisitor,
                                  contextItemType: ContextItemStaticInfo): Expression = {
    var whereClause: WhereClause = null
    var whereIndex: Int = 0
    class WhereClauseStruct {

      var whereIndex: Int = 0

      var whereClause: WhereClause = _

    }
    val whereList: List[WhereClauseStruct] = new ArrayList[WhereClauseStruct]()
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
    if (whereList.size == 0) {
      return null
    }
    while (!whereList.isEmpty) {
      whereClause = whereList.get(0).whereClause
      whereIndex = whereList.get(0).whereIndex
      val condition: Expression = whereClause.getPredicate
      val list: List[Expression] = new ArrayList[Expression](5)
      BooleanExpression.listAndComponents(condition, list)
      var i: Int = list.size - 1
      while (i >= 0) {
        val term: Expression = list.get(i)
        var c: Int = clauses.size - whereIndex - 1
        breakable {
          while (c >= 0) {
            val clause: Clause = clauses.get(c)
            val bindingList: Array[Binding] = clause.getRangeVariables.asInstanceOf[Array[Binding]]
            if (ExpressionTool.dependsOnVariable(term, bindingList) ||
              clause.getClauseKey == Clause.ClauseName.COUNT) {
              val removedExpr: Expression = list.remove(i)
              if (list.isEmpty) {
                clauses.remove(clauses.size - whereIndex)
              } else {
                whereClause.setPredicate(makeAndCondition(list))
              }
              clause match {
                case forClause: ForClause if ! forClause.isAllowingEmpty =>
                  val added: Boolean = forClause.addPredicate(this, visitor, contextItemType, term)
                  if (!added)
                    clauses.add(c + 1, new WhereClause(this, removedExpr))
                case _ =>
                  val newWhere: WhereClause = new WhereClause(this, term)
                  clauses.add(c + 1, newWhere)
              }
              break()
            }
            {
              c -= 1
              c + 1
            }
          }
        }
        if (list.size - 1 == i) {
          list.remove(i)
          if (list.isEmpty) {
            clauses.remove(clauses.size - whereIndex)
          } else {
            whereClause.setPredicate(makeAndCondition(list))
          }
          val newWhere: WhereClause = new WhereClause(this, term)
          clauses.add(0, newWhere)
        }
        {
          i -= 1
          i + 1
        }
      }
      whereList.remove(0)
    }
    this
  }

  private def makeAndCondition(list: List[Expression]): Expression =
    if (list.size == 1) {
      list.get(0)
    } else {
      new AndExpression(list.get(0),
        makeAndCondition(list.subList(1, list.size)))
    }

  private def rewriteForOrLet(
                               visitor: ExpressionVisitor,
                               contextItemType: ContextItemStaticInfo): Expression = {
    var action: Expression = getReturnClause
    var injector: CodeInjector = null
    visitor.getStaticContext match {
      case module: QueryModule =>
        injector = module.getCodeInjector
      case _ =>
    }
    var i: Int = clauses.size - 1
    while (i >= 0) {
      clauses.get(i) match {
        case forClause: ForClause =>
          var forExpr: ForExpression = null
          forExpr =
            if (forClause.isAllowingEmpty) new OuterForExpression()
            else new ForExpression()
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
          val letClause: LetClause = clauses.get(i).asInstanceOf[LetClause]
          val letExpr: LetExpression = new LetExpression()
          letExpr.setLocation(letClause.getLocation)
          letExpr.setRetainedStaticContext(getRetainedStaticContext)
          letExpr.setAction(action)
          letExpr.setSequence(letClause.getSequence)
          letExpr.setVariableQName(letClause.getRangeVariable.getVariableQName)
          letExpr.setRequiredType(letClause.getRangeVariable.getRequiredType)
          if (letClause.getRangeVariable.isIndexedVariable) {
            letExpr.setIndexedVariable()
          }
          ExpressionTool.rebindVariableReferences(action,
            letClause.getRangeVariable,
            letExpr)
          action = letExpr
      }
      {
        i -= 1
        i + 1
      }
    }
    action = action.typeCheck(visitor, contextItemType)
    action = action.optimize(visitor, contextItemType)
    action
  }

  override def iterate(context: XPathContext): SequenceIterator = {
    var stream: TuplePull = new SingularityPull()
    for (c <- clauses.asScala) {
      stream = c.getPullStream(stream, context)
    }
    new ReturnClauseIterator(stream, this, context)
  }

  override def process(output: Outputter, context: XPathContext): Unit = {
    var destination: TuplePush = new ReturnClausePush(output, getReturnClause)
    var i: Int = clauses.size - 1
    while (i >= 0) {
      val c: Clause = clauses.get(i)
      destination = c.getPushStream(destination, output, context)
      i -= 1
    }
    destination.processTuple(context)
    destination.close()
  }

  override def evaluatePendingUpdates(context: XPathContext,
                                      pul: PendingUpdateList): Unit = {
    var stream: TuplePull = new SingularityPull()
    for (c <- clauses.asScala) {
      stream = c.getPullStream(stream, context)
    }
    while (stream.nextTuple(context)) getReturnClause.evaluatePendingUpdates(
      context,
      pul)
  }

  override def getExpressionName: String = "FLWOR"

  override def toShortString: String = {
    val sb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C64)
    sb.append(clauses.get(0).toShortString)
    sb.append(" ... return ")
    sb.append(getReturnClause.toShortString)
    sb.toString
  }

  override def toString: String = {
    val sb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C64)
    for (c <- clauses.asScala) {
      sb.append(c.toString)
      sb.cat(' ')
    }
    sb.append(" return ")
    sb.append(getReturnClause.toString)
    sb.toString
  }

  def hasLoopingVariableReference(binding: Binding): Boolean = {
    var bindingClause: Int = -1
    breakable {
      for (i <- 0 until clauses.size
           if clauseHasBinding(clauses.get(i), binding)) {
        bindingClause = i
        break()
      }
    }
    val boundOutside: Boolean = bindingClause < 0
    if (boundOutside) {
      bindingClause = 0
    }
    var lastReferencingClause: Int = clauses.size
    if (!ExpressionTool.dependsOnVariable(getReturnClause, Array(binding))) {
      val response: List[Boolean] = new ArrayList[Boolean]()
      val checker: OperandProcessor = op =>
        if (response.isEmpty &&
          ExpressionTool.dependsOnVariable(op.getChildExpression,
            Array(binding))) {
          response.add(true)
        }
      var i: Int = clauses.size - 1
      breakable {
        while (i >= 0) {
          try {
            clauses.get(i).processOperands(checker)
            if (!response.isEmpty) {
              lastReferencingClause = i
              break()
            }
          } catch {
            case e: XPathException => assert(false)
          }
          i -= 1
        }
      }
    }
    var i: Int = lastReferencingClause - 1
    while (i >= bindingClause) {
      if (isLoopingClause(clauses.get(i)))
        return true
      i -= 1
    }
    false
  }
}
