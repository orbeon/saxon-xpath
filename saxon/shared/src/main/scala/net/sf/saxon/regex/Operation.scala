package net.sf.saxon.regex

import net.sf.saxon.expr.sort.EmptyIntIterator

import net.sf.saxon.regex.charclass.CharacterClass

import net.sf.saxon.regex.charclass.EmptyCharacterClass

import net.sf.saxon.regex.charclass.IntSetCharacterClass

import net.sf.saxon.regex.charclass.SingletonCharacterClass

import net.sf.saxon.trans.UncheckedXPathException

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.FastStringBuffer

import net.sf.saxon.z._

import java.util.Iterator

import java.util.List

import java.util.Stack

import java.util.function.IntPredicate

import scala.jdk.CollectionConverters._

import scala.beans.{BeanProperty, BooleanBeanProperty}


object Operation {

  val MATCHES_ZLS_AT_START: Int = 1

  val MATCHES_ZLS_AT_END: Int = 2

  val MATCHES_ZLS_ANYWHERE: Int = 7

  val MATCHES_ZLS_NEVER: Int = 1024

  class OpChoice(var branches: List[Operation]) extends Operation {

    override def getMatchLength(): Int = {
      val fixed: Int = branches.get(0).getMatchLength
      for (i <- 1 until branches.size
           if branches.get(i).getMatchLength != fixed) {
        -1
      }
      fixed
    }

    override def getMinimumMatchLength(): Int = {
      var min: Int = branches.get(0).getMinimumMatchLength
      for (i <- 1 until branches.size) {
        val m: Int = branches.get(i).getMinimumMatchLength
        if (m < min) {
          min = m
        }
      }
      min
    }

    override def matchesEmptyString(): Int = {
      var m: Int = 0
      for (branch <- branches.asScala) {
        val b: Int = branch.matchesEmptyString()
        if (b != MATCHES_ZLS_NEVER) {
          m |= b
        }
      }
      m
    }

    override def containsCapturingExpressions(): Boolean =
      branches.asScala
        .find(o =>
          o.isInstanceOf[OpCapture] || o.containsCapturingExpressions())
        .map(_ => true)
        .getOrElse(false)

    override def getInitialCharacterClass(caseBlind: Boolean): CharacterClass = {
      var result: CharacterClass = EmptyCharacterClass.getInstance
      for (o <- branches.asScala) {
        result =
          RECompiler.makeUnion(result, o.getInitialCharacterClass(caseBlind))
      }
      result
    }

    override def optimize(program: REProgram, flags: REFlags): Operation = {
      for (i <- 0 until branches.size) {
        val o1: Operation = branches.get(i)
        val o2: Operation = o1.optimize(program, flags)
        if (o1 != o2) {
          branches.set(i, o2)
        }
      }
      this
    }

    override def iterateMatches(matcher: REMatcher, position: Int): IntIterator = new IntIterator() {
      var branchIter: Iterator[Operation] = branches.iterator()

      var currentIter: IntIterator = null

      var currentOp: Operation = null

      def hasNext(): Boolean = {
        while (true) {
          if (currentIter == null) {
            if (branchIter.hasNext) {
              matcher.clearCapturedGroupsBeyond(position)
              currentOp = branchIter.next()
              currentIter = currentOp.iterateMatches(matcher, position)
            } else {
              return false
            }
          }
          if (currentIter.hasNext) {
            true
          } else {
            currentIter = null
          }
        }
        return false
      }

      def next(): Integer = currentIter.next
    }

    override def display(): String = {
      val fsb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C64)
      fsb.append("(?:")
      var first: Boolean = true
      for (branch <- branches.asScala) {
        if (first) {
          first = false
        } else {
          fsb.cat('|')
        }
        fsb.append(branch.display())
      }
      fsb.append(")")
      fsb.toString
    }

  }

  class OpSequence(@BeanProperty var operations: List[Operation])
    extends Operation {

    override def getMatchLength(): Int = {
      var len: Int = 0
      for (o <- operations.asScala) {
        val i: Int = o.getMatchLength
        if (i == -1) {
          -1
        }
        len += i
      }
      len
    }

    override def getMinimumMatchLength(): Int = {
      var len: Int = 0
      for (o <- operations.asScala) {
        len += o.getMinimumMatchLength
      }
      len
    }

    override def matchesEmptyString(): Int = {
      var matchesEmptyAnywhere: Boolean = true
      val matchesEmptyNowhere: Boolean = false
      for (o <- operations.asScala) {
        val m: Int = o.matchesEmptyString()
        if (m == MATCHES_ZLS_NEVER) {
          MATCHES_ZLS_NEVER
        }
        if (m != MATCHES_ZLS_ANYWHERE) {
          matchesEmptyAnywhere = false
          //break
        }
      }
      if (matchesEmptyAnywhere) {
        MATCHES_ZLS_ANYWHERE
      }
      var matchesBOL: Boolean = true
      for (o <- operations.asScala
           if (o.matchesEmptyString() & MATCHES_ZLS_AT_START) == 0) {
        matchesBOL = false
        //break
      }
      if (matchesBOL) {
        MATCHES_ZLS_AT_START
      }
      var matchesEOL: Boolean = true
      for (o <- operations.asScala
           if (o.matchesEmptyString() & MATCHES_ZLS_AT_END) == 0) {
        matchesEOL = false
        //break
      }
      if (matchesEOL) {
        MATCHES_ZLS_AT_END
      }
      0
    }

    override def containsCapturingExpressions(): Boolean =
      operations.asScala
        .find(o =>
          o.isInstanceOf[OpCapture] || o.containsCapturingExpressions())
        .map(_ => true)
        .getOrElse(false)

    override def getInitialCharacterClass(caseBlind: Boolean): CharacterClass = {
      var result: CharacterClass = EmptyCharacterClass.getInstance
      for (o <- operations.asScala) {
        result =
          RECompiler.makeUnion(result, o.getInitialCharacterClass(caseBlind))
        if (o.matchesEmptyString() == MATCHES_ZLS_NEVER) {
          result
        }
      }
      result
    }

    override def display(): String = {
      val fsb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C64)
      for (op <- operations.asScala) {
        fsb.append(op.display())
      }
      fsb.toString
    }

    override def optimize(program: REProgram, flags: REFlags): Operation =
      if (operations.size == 0) {
        new OpNothing()
      } else if (operations.size == 1) {
        operations.get(0)
      } else {
        for (i <- 0 until operations.size - 1) {
          val o1: Operation = operations.get(i)
          val o2: Operation = o1.optimize(program, flags)
          if (o1 != o2) {
            operations.set(i, o2)
          }
          if (o2.isInstanceOf[OpRepeat]) {
            val o1r: Operation = o1.asInstanceOf[OpRepeat].getRepeatedOperation
            if (o1r.isInstanceOf[OpAtom] || o1r.isInstanceOf[OpCharClass]) {
              val o2r: Operation = operations.get(i + 1)
              if (o1.asInstanceOf[OpRepeat]
                .min == o1.asInstanceOf[OpRepeat].max ||
                RECompiler.noAmbiguity(o1r,
                  o2r,
                  flags.isCaseIndependent,
                  !o1.asInstanceOf[OpRepeat].greedy)) {
                operations.set(
                  i,
                  new OpUnambiguousRepeat(o1r,
                    o1.asInstanceOf[OpRepeat].min,
                    o1.asInstanceOf[OpRepeat].max))
              }
            }
          }
        }
        this
      }

    override def iterateMatches(matcher: REMatcher,
                                position: Int): IntIterator = {
      val iterators: Stack[IntIterator] = new Stack[IntIterator]()
      val savedState: REMatcher.State =
        if (containsCapturingExpressions()) matcher.captureState else null
      val backtrackingLimit: Int = matcher.getProgram.getBacktrackingLimit
      new IntIterator() {
        private var primed: Boolean = false

        private var nextPos: Int = _

        private def advance(): Int = {
          var counter: Int = 0
          while (!iterators.isEmpty) {
            var top: IntIterator = iterators.peek()
            while (top.hasNext) {
              val p: Int = top.next
              matcher.clearCapturedGroupsBeyond(p)
              val i: Int = iterators.size
              if (i >= operations.size) {
                p
              }
              top = operations.get(i).iterateMatches(matcher, p)
              iterators.push(top)
            }
            iterators.pop()
            if (backtrackingLimit >= 0 && {
              counter += 1;
              counter - 1
            } > backtrackingLimit) {
              throw new UncheckedXPathException(
                new XPathException(
                  "Regex backtracking limit exceeded processing " + matcher.operation
                    .display() +
                    ". Simplify the regular expression, " +
                    "or set Feature.REGEX_BACKTRACKING_LIMIT to -1 to remove this limit."))
            }
          }
          if (savedState != null) {
            matcher.resetState(savedState)
          }
          -1
        }

        def hasNext(): Boolean = {
          if (!primed) {
            iterators.push(operations.get(0).iterateMatches(matcher, position))
            primed = true
          }
          nextPos = advance()
          nextPos >= 0
        }

        def next(): Integer = nextPos
      }
    }

  }

  class OpCharClass(@BeanProperty var predicate: IntPredicate)
    extends Operation {

    override def getMatchLength(): Int = 1

    override def matchesEmptyString(): Int = MATCHES_ZLS_NEVER

    override def getInitialCharacterClass(caseBlind: Boolean): CharacterClass =
      if (predicate.isInstanceOf[CharacterClass]) {
        predicate.asInstanceOf[CharacterClass]
      } else {
        super.getInitialCharacterClass(caseBlind)
      }

    override def iterateMatches(matcher: REMatcher,
                                position: Int): IntIterator = {
      val in: UnicodeString = matcher.search
      if (position < in.uLength() && predicate.test(in.uCharAt(position))) {
        new IntSingletonIterator(position + 1)
      } else {
        EmptyIntIterator.getInstance
      }
    }

    override def display(): String =
      if (predicate.isInstanceOf[IntSetPredicate]) {
        val s: IntSet = predicate.asInstanceOf[IntSetPredicate].getIntSet
        if (s.isInstanceOf[IntSingletonSet]) {
          "" + s.asInstanceOf[IntSingletonSet].getMember.toChar
        } else if (s.isInstanceOf[IntRangeSet]) {
          val fsb: FastStringBuffer = new FastStringBuffer(
            FastStringBuffer.C64)
          val irs: IntRangeSet = s.asInstanceOf[IntRangeSet]
          fsb.append("[")
          for (i <- 0 until irs.getNumberOfRanges) {
            fsb.cat(irs.getStartPoints()(1).toChar)
            fsb.append("-")
            fsb.cat(irs.getEndPoints()(1).toChar)
          }
          fsb.append("[")
          fsb.toString
        } else {
          "[....]"
        }
      } else {
        "[....]"
      }

  }

  class OpAtom(@BeanProperty var atom: UnicodeString) extends Operation {

    private var len: Int = atom.uLength()

    override def getMatchLength(): Int = len

    override def matchesEmptyString(): Int =
      if (len == 0) MATCHES_ZLS_ANYWHERE else MATCHES_ZLS_NEVER

    override def getInitialCharacterClass(caseBlind: Boolean): CharacterClass = {
      if (len == 0) {
        EmptyCharacterClass.getInstance
      } else if (caseBlind) {
        var set: IntSet = null
        val ch: Int = atom.uCharAt(0)
        val variants: Array[Int] = CaseVariants.getCaseVariants(ch)
        if (variants.length > 0) {
          set = new IntHashSet(variants.length)
          set.add(ch)
          for (v <- variants) {
            set.add(v)
          }
          new IntSetCharacterClass(set)
        }
      }
      new SingletonCharacterClass(atom.uCharAt(0))
    }

    override def iterateMatches(matcher: REMatcher,
                                position: Int): IntIterator = {
      val in: UnicodeString = matcher.search
      if (position + len > in.uLength()) {
        EmptyIntIterator.getInstance
      }
      if (matcher.program.flags.isCaseIndependent) {
        for (i <- 0 until len
             if !matcher.equalCaseBlind(in.uCharAt(position + i),
               atom.uCharAt(i))) {
          EmptyIntIterator.getInstance
        }
      } else {
        for (i <- 0 until len if in.uCharAt(position + i) != atom.uCharAt(i)) {
          EmptyIntIterator.getInstance
        }
      }
      new IntSingletonIterator(position + len)
    }

    def display(): String = atom.toString

  }

  class OpGreedyFixed(op: Operation, min: Int, max: Int, private var len: Int)
    extends OpRepeat(op, min, max, true) {

    var opt = op

    override def getMatchLength(): Int = if (min == max) min * len else -1

    override def matchesEmptyString(): Int = {
      if (min == 0) {
        return MATCHES_ZLS_ANYWHERE
      }
      opt.matchesEmptyString()
    }

    override def optimize(program: REProgram, flags: REFlags): Operation = {
      if (max == 0) {
        new OpNothing()
      }
      if (opt.getMatchLength == 0) {
        return opt
      }
      opt = opt.optimize(program, flags)
      this
    }

    override def iterateMatches(matcher: REMatcher,
                                position: Int): IntIterator = {
      var guard: Int = matcher.search.uLength()
      if (max < java.lang.Integer.MAX_VALUE) {
        guard = java.lang.Math.min(guard, position + len * max)
      }
      if (position >= guard && min > 0) {
        EmptyIntIterator.getInstance
      }
      var p: Int = position
      var matches: Int = 0
      while (p <= guard) {
        val it: IntIterator = opt.iterateMatches(matcher, p)
        var matched: Boolean = false
        if (it.hasNext) {
          matched = true
          it.next
        }
        if (matched) {
          {
            matches += 1;
            matches - 1
          }
          p += len
          if (matches == max) {
            //break
          }
        } else {
          //break
        }
      }
      if (matches < min) {
        EmptyIntIterator.getInstance
      }
      new IntStepIterator(p, -len, position + len * min)
    }

  }

  class OpUnambiguousRepeat(op: Operation, min: Int, max: Int)
    extends OpRepeat(op, min, max, true) {
    var opt = op

    override def matchesEmptyString(): Int = {
      if (min == 0) {
        return MATCHES_ZLS_ANYWHERE
      }
      opt.matchesEmptyString()
    }

    override def getMatchLength(): Int =
      if (opt.getMatchLength != -1 && min == max) {
        opt.getMatchLength * min
      } else {
        -1
      }

    override def optimize(program: REProgram, flags: REFlags): Operation = {
      opt = opt.optimize(program, flags)
      this
    }

    override def iterateMatches(matcher: REMatcher,
                                position: Int): IntIterator = {
      val guard: Int = matcher.search.uLength()
      var p: Int = position
      var matches: Int = 0
      while (matches < max && p <= guard) {
        val it: IntIterator = opt.iterateMatches(matcher, p)
        if (it.hasNext) {
          {
            matches += 1;
            matches - 1
          }
          p = it.next
        } else {
          //break
        }
      }
      if (matches < min) {
        EmptyIntIterator.getInstance
      } else {
        new IntSingletonIterator(p)
      }
    }

  }

  class OpRepeat(var op: Operation,
                 var min: Int,
                 var max: Int,
                 var greedy: Boolean)
    extends Operation {

    def getRepeatedOperation(): Operation = op

    override def matchesEmptyString(): Int = {
      if (min == 0) {
        return MATCHES_ZLS_ANYWHERE
      }
      op.matchesEmptyString()
    }

    override def containsCapturingExpressions(): Boolean =
      op.isInstanceOf[OpCapture] || op.containsCapturingExpressions()

    override def getInitialCharacterClass(caseBlind: Boolean): CharacterClass =
      op.getInitialCharacterClass(caseBlind)

    override def getMatchLength(): Int =
      if (min == max && op.getMatchLength >= 0) min * op.getMatchLength else -1

    override def getMinimumMatchLength(): Int = min * op.getMinimumMatchLength

    override def optimize(program: REProgram, flags: REFlags): Operation = {
      op = op.optimize(program, flags)
      if (min == 0 && op.matchesEmptyString() == MATCHES_ZLS_ANYWHERE) {
        min = 1
      }
      this
    }

    override def iterateMatches(matcher: REMatcher,
                                position: Int): IntIterator = {
      val iterators: Stack[IntIterator] = new Stack[IntIterator]()
      val positions: Stack[Integer] = new Stack[Integer]()
      val bound: Int = Math.min(max, matcher.search.uLength() - position + 1)
      var p: Int = position
      if (greedy) {
        if (min == 0 &&
          !matcher.history.isDuplicateZeroLengthMatch(this, position)) {
          iterators.push(new IntSingletonIterator(position))
          positions.push(p)
        }
        for (i <- 0 until bound) {
          val it: IntIterator = op.iterateMatches(matcher, p)
          if (it.hasNext) {
            p = it.next
            iterators.push(it)
            positions.push(p)
          } else if (iterators.isEmpty) {
            EmptyIntIterator.getInstance
          } else {
            //break
          }
        }
        val base: IntIterator = new IntIterator() {
          var primed: Boolean = true

          private def advance(): Unit = {
            var top: IntIterator = iterators.peek()
            if (top.hasNext) {
              var p: Int = top.next
              positions.pop()
              positions.push(p)
              while (iterators.size < bound) {
                var it: IntIterator = op.iterateMatches(matcher, p)
                if (it.hasNext) {
                  p = it.next
                  iterators.push(it)
                  positions.push(p)
                } else {
                  //break
                }
              }
            } else {
              iterators.pop()
              positions.pop()
            }
          }

          def hasNext(): Boolean =
            if (primed && iterators.size >= min) {
              !iterators.isEmpty
            } else if (iterators.isEmpty) {
              false
            } else {
              do advance() while (iterators.size < min && !iterators.isEmpty);
              !iterators.isEmpty
            }

          def next(): Integer = {
            primed = false
            positions.peek()
          }
        }
        new ForceProgressIterator(base)
      } else {
        val iter: IntIterator = new IntIterator() {
          private var pos: Int = position

          private var counter: Int = 0

          private def advance(): Unit = {
            var it: IntIterator = op.iterateMatches(matcher, pos)
            if (it.hasNext) {
              pos = it.next
              if ( {
                {
                  counter += 1
                  counter
                } > max
              }) {
                pos = -1
              }
            } else if (min == 0 && counter == 0) {
              {
                counter += 1;
                counter - 1
              }
            } else {
              pos = -1
            }
          }

          def hasNext(): Boolean = {
            do advance() while (counter < min && pos >= 0);
            pos >= 0
          }

          def next(): Integer = pos
        }
        new ForceProgressIterator(iter)
      }
    }

    override def display(): String = {
      var quantifier: String = null
      quantifier =
        if (min == 0 && max == java.lang.Integer.MAX_VALUE) "*"
        else if (min == 1 && max == java.lang.Integer.MAX_VALUE) "+"
        else if (min == 0 && max == 1) "?"
        else "{" + min + "," + max + "}"
      if (!greedy) {
        quantifier += "?"
      }
      op.display() + quantifier
    }

  }

  class OpReluctantFixed(op: Operation,
                         min: Int,
                         max: Int,
                         private var len: Int)
    extends OpRepeat(op, min, max, false) {
    var opt = op

    override def getMatchLength(): Int = if (min == max) min * len else -1

    override def matchesEmptyString(): Int = {
      if (min == 0) {
        return MATCHES_ZLS_ANYWHERE
      }
      opt.matchesEmptyString()
    }

    override def optimize(program: REProgram, flags: REFlags): Operation = {
      opt = opt.optimize(program, flags)
      this
    }

    override def iterateMatches(matcher: REMatcher,
                                position: Int): IntIterator =
      new IntIterator() {
        private var pos: Int = position

        private var count: Int = 0

        private var started: Boolean = false

        def hasNext(): Boolean = {
          if (!started) {
            started = true
            while (count < min) {
              val child: IntIterator = opt.iterateMatches(matcher, pos)
              if (child.hasNext) {
                pos = child.next
                count += 1
              } else {
                return false
              }
            }
            return true
          }
          if (count < max) {
            matcher.clearCapturedGroupsBeyond(pos)
            val child: IntIterator = opt.iterateMatches(matcher, pos)
            if (child.hasNext) {
              pos = child.next
              count += 1
              return true
            }
          }
          false
        }

        def next(): Integer = pos
      }

  }

  class OpEndProgram extends Operation {

    override def getMatchLength(): Int = 0

    override def matchesEmptyString(): Int = MATCHES_ZLS_ANYWHERE

    override def iterateMatches(matcher: REMatcher,
                                position: Int): IntIterator =
      if (matcher.anchoredMatch) {
        if (matcher.search.isEnd(position)) {
          new IntSingletonIterator(position)
        } else {
          EmptyIntIterator.getInstance
        }
      } else {
        matcher.setParenEnd(0, position)
        new IntSingletonIterator(position)
      }

    override def display(): String = "\\Z"

  }

  class OpBOL extends Operation {

    override def getMatchLength(): Int = 0

    override def matchesEmptyString(): Int = MATCHES_ZLS_AT_START

    override def iterateMatches(matcher: REMatcher,
                                position: Int): IntIterator = {
      if (position != 0) {
        if (matcher.program.flags.isMultiLine) {
          if (matcher.isNewline(position - 1) && !matcher.search.isEnd(
            position)) {
            new IntSingletonIterator(position)
          }
        }
        EmptyIntIterator.getInstance
      }
      new IntSingletonIterator(position)
    }

    override def display(): String = "^"

  }

  class OpEOL extends Operation {

    override def getMatchLength(): Int = 0

    override def matchesEmptyString(): Int = MATCHES_ZLS_AT_END

    override def iterateMatches(matcher: REMatcher,
                                position: Int): IntIterator = {
      val search: UnicodeString = matcher.search
      if (matcher.program.flags.isMultiLine) {
        if (search.isEnd(0) || search.isEnd(position) || matcher.isNewline(
          position)) {
          new IntSingletonIterator(position)
        } else {
          EmptyIntIterator.getInstance
        }
      } else {
        if (search.isEnd(0) || search.isEnd(position)) {
          new IntSingletonIterator(position)
        } else {
          EmptyIntIterator.getInstance
        }
      }
    }

    override def display(): String = "$"

  }

  class OpCapture(var childOp: Operation, group: Int) extends Operation {

    var groupNr: Int = group

    override def getMatchLength(): Int = childOp.getMatchLength

    override def getMinimumMatchLength(): Int = childOp.getMinimumMatchLength

    override def matchesEmptyString(): Int = childOp.matchesEmptyString()

    override def optimize(program: REProgram, flags: REFlags): Operation = {
      childOp = childOp.optimize(program, flags)
      this
    }

    override def iterateMatches(matcher: REMatcher,
                                position: Int): IntIterator = {
      if ((matcher.program.optimizationFlags & REProgram.OPT_HASBACKREFS) !=
        0) {
        matcher.startBackref(groupNr) = position
      }
      val base: IntIterator = childOp.iterateMatches(matcher, position)
      new IntIterator() {
        def hasNext(): Boolean = base.hasNext

        def next(): Integer = {
          val next: Int = base.next
          if (groupNr >= matcher.captureState.parenCount) {
            matcher.captureState.parenCount = groupNr + 1
          }
          matcher.setParenStart(groupNr, position)
          matcher.setParenEnd(groupNr, next)
          if ((matcher.program.optimizationFlags & REProgram.OPT_HASBACKREFS) !=
            0) {
            matcher.startBackref(groupNr) = position
            matcher.endBackref(groupNr) = next
          }
          next
        }
      }
    }

    override def display(): String = "(" + childOp.display() + ")"

  }

  class OpBackReference(var groupNr: Int) extends Operation {

    override def matchesEmptyString(): Int = 0

    override def iterateMatches(matcher: REMatcher,
                                position: Int): IntIterator = {
      val s: Int = matcher.startBackref(groupNr)
      val e: Int = matcher.endBackref(groupNr)
      if (s == -1 || e == -1) {
        EmptyIntIterator.getInstance
      }
      if (s == e) {
        new IntSingletonIterator(position)
      }
      val l: Int = e - s
      val search: UnicodeString = matcher.search
      if (search.isEnd(position + l - 1)) {
        EmptyIntIterator.getInstance
      }
      if (matcher.program.flags.isCaseIndependent) {
        for (i <- 0 until l
             if !matcher.equalCaseBlind(search.uCharAt(position + i),
               search.uCharAt(s + i))) {
          EmptyIntIterator.getInstance
        }
      } else {
        for (i <- 0 until l
             if search.uCharAt(position + i) != search.uCharAt(s + i)) {
          EmptyIntIterator.getInstance
        }
      }
      new IntSingletonIterator(position + l)
    }

    override def display(): String = "\\" + groupNr

  }

  class OpNothing extends Operation {

    def iterateMatches(matcher: REMatcher, position: Int): IntIterator =
      new IntSingletonIterator(position)

    override def matchesEmptyString(): Int = MATCHES_ZLS_ANYWHERE

    override def getMatchLength(): Int = 0

    override def display(): String = "()"

  }

  object OpTrace {

    var counter: Int = 0

  }

  class OpTrace(var base: Operation) extends Operation {

    var counter: Int = 0

    override def iterateMatches(matcher: REMatcher,
                                position: Int): IntIterator = {
      val baseIter: IntIterator = base.iterateMatches(matcher, position)
      val iterNr: Int = {
        counter += 1;
        counter
      }
      val clName: String = baseIter.getClass.getName
      val lastDot: Int = clName.lastIndexOf(".")
      val iterName: String = clName.substring(lastDot + 1)
      System.err.println(
        "Iterating over " + base.getClass.getSimpleName + " " +
          base.display() +
          " at position " +
          position +
          " returning " +
          iterName +
          " " +
          iterNr)
      new IntIterator() {
        def hasNext(): Boolean = {
          val b: Boolean = baseIter.hasNext
          System.err.println("IntIterator " + iterNr + " hasNext() = " + b)
          b
        }

        def next(): Integer = {
          val n: Int = baseIter.next
          System.err.println("IntIterator " + iterNr + " next() = " + n)
          n
        }
      }
    }

    override def getMatchLength(): Int = base.getMatchLength

    override def matchesEmptyString(): Int = base.matchesEmptyString()

    override def optimize(program: REProgram, flags: REFlags): Operation = {
      base = base.optimize(program, flags)
      this
    }

    override def display(): String = base.display()

  }

  private class ForceProgressIterator(private var base: IntIterator)
    extends IntIterator {

    var countZeroLength: Int = 0

    var currentPos: Int = -1

    def hasNext(): Boolean = countZeroLength <= 3 && base.hasNext

    def next(): Integer = {
      val p: Int = base.next
      if (p == currentPos) {
        countZeroLength += 1
      } else {
        countZeroLength = 0
        currentPos = p
      }
      p
    }

  }

}

abstract class Operation {

  def iterateMatches(matcher: REMatcher, position: Int): IntIterator

  def getMatchLength(): Int = -1

  def getMinimumMatchLength(): Int = {
    val fixed: Int = getMatchLength
    if (fixed < 0) 0 else fixed
  }

  def matchesEmptyString(): Int

  def containsCapturingExpressions(): Boolean = false

  def getInitialCharacterClass(caseBlind: Boolean): CharacterClass =
    EmptyCharacterClass.getComplement

  def optimize(program: REProgram, flags: REFlags): Operation = this

  def display(): String

}
