package org.orbeon.saxon

class FlworAndQuantifiedTest extends XPathTestBase {

  describe("XPath For Expressions") {

    describe("Basic for expressions") {
      checkStrings("for $x in (1, 2, 3) return $x * 2", List("2", "4", "6"))
      checkStrings("for $x in ('a', 'b', 'c') return concat($x, '!')", List("a!", "b!", "c!"))
      checkStrings("for $x in 1 to 5 return $x", List("1", "2", "3", "4", "5"))
      checkStrings("for $x in () return $x + 1", Nil)

      // Multiple variables (cartesian product)
      checkStrings(
        "for $x in (1, 2), $y in (10, 20) return $x + $y",
        List("11", "21", "12", "22")
      )
      checkStrings(
        "for $a in ('x', 'y'), $b in (1, 2) return concat($a, $b)",
        List("x1", "x2", "y1", "y2")
      )

      // Dependent for clauses
      checkStrings(
        "for $x in (1, 2, 3), $y in 1 to $x return concat($x, '.', $y)",
        List("1.1", "2.1", "2.2", "3.1", "3.2", "3.3")
      )

      // Three variables
      checkStrings(
        "for $x in (1, 2), $y in (10, 20), $z in (100, 200) return $x + $y + $z",
        List("111", "211", "121", "221", "112", "212", "122", "222")
      )
    }

    describe("Let expressions") {
      check("let $x := 10 return $x * 2", "20")
      check("let $name := 'world' return concat('Hello, ', $name, '!')", "Hello, world!")
      checkStrings("let $seq := (1, 2, 3) return $seq", List("1", "2", "3"))
      check("let $seq := (1, 2, 3) return count($seq)", "3")
      check("let $seq := (1, 2, 3) return sum($seq)", "6")

      // Multiple let bindings
      check(
        """
          let $a := 5,
              $b := 10,
              $c := $a + $b
          return $c * 2
        """.trim,
        "30"
      )

      check("let $x := () return empty($x)", "true")
      check("let $x := () return count($x)", "0")

      // Let binding shadowing
      check(
        "let $x := 5, $x := $x * 2 return $x",
        "10"
      )
    }

    describe("Nested for and let expressions") {
      checkStrings(
        """
          for $x in (1, 2, 3)
          return let $sq := $x * $x
                 return concat($x, '^2=', $sq)
        """.trim,
        List("1^2=1", "2^2=4", "3^2=9")
      )

      checkStrings(
        """
          let $factor := 10
          return for $x in (1, 2, 3)
                 return $x * $factor
        """.trim,
        List("10", "20", "30")
      )

      checkStrings(
        """
          let $base := 100
          return for $i in (1, 2)
                 return let $sub := $base + $i
                        return for $j in (10, 20)
                               return $sub + $j
        """.trim,
        List("111", "121", "112", "122")
      )
    }

    describe("Filtering in for expressions") {
      checkStrings(
        "for $x in (1 to 10)[. mod 2 = 0] return $x",
        List("2", "4", "6", "8", "10")
      )
      checkStrings(
        "for $x in (1 to 10)[. > 7] return $x",
        List("8", "9", "10")
      )

      checkStrings(
        "for $x in 1 to 20 return if ($x mod 2 = 0 and $x mod 3 = 0) then $x else ()",
        List("6", "12", "18")
      )

      checkStrings(
        """
          for $x in (1, 2, 3),
              $y in (1, 2, 3)
          return if ($x < $y) then concat($x, '<', $y) else ()
        """.trim,
        List("1<2", "1<3", "2<3")
      )

      checkStrings("for $x in (1 to 5)[. > 10] return $x", Nil)

      checkStrings(
        "for $x in ('', 'a', '', 'b') return if (string-length($x) > 0) then $x else ()",
        List("a", "b")
      )
    }

    describe("fn:sort function (XPath 3.1)") {
      checkStrings(
        "sort((3, 1, 4, 1, 5, 9, 2))",
        List("1", "1", "2", "3", "4", "5", "9")
      )

      checkStrings(
        "sort(('banana', 'apple', 'cherry'))",
        List("apple", "banana", "cherry")
      )

      // Reverse sort using 3-arg sort with key function
      checkStrings(
        "sort((1, 4, 2, 5, 3), (), function($x) { -$x })",
        List("5", "4", "3", "2", "1")
      )

      // Sort strings by length
      checkStrings(
        "sort(('elephant', 'cat', 'giraffe', 'dog'), (), string-length#1)",
        List("cat", "dog", "giraffe", "elephant")
      )

      // Sort maps/arrays by property
      checkStrings(
        """
          let $data := (
            map {'id': 2, 'name': 'Bob'},
            map {'id': 1, 'name': 'Alice'},
            map {'id': 3, 'name': 'Charlie'}
          )
          return sort($data, (), function($m) { $m('name') }) ! .('name')
        """.trim,
        List("Alice", "Bob", "Charlie")
      )

      checkStrings(
        """
          let $data := (
            map {'id': 2, 'name': 'Bob'},
            map {'id': 1, 'name': 'Alice'},
            map {'id': 3, 'name': 'Charlie'}
          )
          return sort($data, (), function($m) { $m('id') }) ! .('name')
        """.trim,
        List("Alice", "Bob", "Charlie")
      )
    }

    describe("Nested for with maps, arrays, and functions") {
      checkStrings(
        """
          for $x in (1, 2)
          return for $y in ('a', 'b')
                 return concat($x, $y)
        """.trim,
        List("1a", "1b", "2a", "2b")
      )

      check(
        """
          let $evens := (for $x in (1 to 10)[. mod 2 = 0] return $x)
          return sum($evens)
        """.trim,
        "30"
      )

      check(
        "string-join(for $x in (1, 2, 3) return string($x * 10), ', ')",
        "10, 20, 30"
      )

      // Square array constructor creates a single member for the whole enclosed expression
      check(
        "array:size([for $x in 1 to 5 return $x * 2])",
        "1"
      )
      checkStrings(
        "[for $x in 1 to 3 return $x](1)",
        List("1", "2", "3")
      )
      // Curly array constructor creates a member for each item in the sequence
      check(
        "array:size(array { for $x in 1 to 5 return $x * 2 })",
        "5"
      )
      check(
        "array { for $x in 1 to 3 return $x }(2)",
        "2"
      )

      check(
        """
          let $m := map:merge(
            for $k in ('a', 'b', 'c')
            return map:entry($k, string-length($k))
          )
          return $m('b')
        """.trim,
        "1"
      )
    }
  }

  describe("Quantified Expressions") {

    describe("some ... in ... satisfies ...") {
      check("some $x in (1, 2, 3, 4) satisfies $x > 3", "true")
      check("some $x in (1, 2, 3, 4) satisfies $x = 2", "true")
      check("some $s in ('apple', 'banana', 'cherry') satisfies starts-with($s, 'b')", "true")

      check("some $x in (1, 2, 3, 4) satisfies $x > 10", "false")
      check("some $x in (1, 2, 3, 4) satisfies $x < 0", "false")
      check("some $s in ('apple', 'banana') satisfies starts-with($s, 'z')", "false")

      check("some $x in () satisfies $x > 0", "false")
      check("some $x in () satisfies true()", "false")

      check(
        "some $x in (1, 2, 3), $y in (3, 4, 5) satisfies $x = $y",
        "true"
      )
      check(
        "some $x in (1, 2), $y in (10, 20) satisfies $x + $y = 21",
        "true"
      )
      check(
        "some $x in (1, 2), $y in (10, 20) satisfies $x + $y = 99",
        "false"
      )

      check("some $x in ('', '', 'non-empty') satisfies string-length($x) > 0", "true")
      check("some $x in ('', '') satisfies string-length($x) > 0", "false")
      check("some $x in (0, 0, 1) satisfies $x = 1", "true")
      check("some $x in (0, 0, 0) satisfies $x = 1", "false")
    }

    describe("every ... in ... satisfies ...") {
      check("every $x in (1, 2, 3) satisfies $x > 0", "true")
      check("every $x in (2, 4, 6) satisfies $x mod 2 = 0", "true")
      check("every $s in ('cat', 'dog', 'cow') satisfies string-length($s) = 3", "true")

      check("every $x in (1, 2, 3, -1) satisfies $x > 0", "false")
      check("every $x in (2, 4, 5, 6) satisfies $x mod 2 = 0", "false")
      check("every $s in ('cat', 'horse') satisfies string-length($s) = 3", "false")

      check("every $x in () satisfies $x > 100", "true")
      check("every $x in () satisfies false()", "true")

      check(
        "every $x in (1, 2), $y in (10, 20) satisfies $x + $y > 10",
        "true"
      )
      check(
        "every $x in (1, 2), $y in (10, 20) satisfies $x + $y > 15",
        "false"
      )

      check("every $x in ('a', 'bb', 'ccc') satisfies string-length($x) > 0", "true")
      check("every $x in ('a', '', 'ccc') satisfies string-length($x) > 0", "false")
      check("every $x in (5, 6, 7) satisfies $x > 4", "true")
      check("every $x in (5, 0, 7) satisfies $x > 4", "false")
    }

    describe("Combined and nested quantified expressions") {
      check(
        """
          every $row in ([1, 2], [3, 4])
          satisfies some $val in $row?*
          satisfies $val >= 2
        """.trim,
        "true"
      )
      check(
        """
          some $row in ([1, 2], [3, 4])
          satisfies every $val in $row?*
          satisfies $val > 2
        """.trim,
        "true"
      )
      check(
        """
          every $row in ([1, 2], [3, 4])
          satisfies every $val in $row?*
          satisfies $val > 2
        """.trim,
        "false"
      )

      // Short circuit
      check("every $x in (false(), error()) satisfies $x", "false")
      check("some $x in (true(), error()) satisfies $x", "true")

      // Empty sequences in nested
      check(
        "every $x in (1, 2, 3) satisfies (every $y in () satisfies false())",
        "true"
      )
      check(
        "some $x in (1, 2, 3) satisfies (some $y in () satisfies true())",
        "false"
      )
    }
  }
}
