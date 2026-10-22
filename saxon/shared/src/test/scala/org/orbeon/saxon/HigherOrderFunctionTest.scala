package org.orbeon.saxon

class HigherOrderFunctionTest extends XPathTestBase {

  describe("XPath 3.1 Higher-Order Functions") {

    describe("Inline function expressions") {
      check("let $f := function() { 42 } return $f()", "42")
      check("let $f := function($x) { $x * 2 } return $f(21)", "42")
      check("let $f := function($x, $y) { $x + $y } return $f(19, 23)", "42")
      check("let $f := function($a, $b, $c) { concat($a, $b, $c) } return $f('X', 'Y', 'Z')", "XYZ")
      // Immediate invocation of inline function
      check("(function($x) { $x * $x })(7)", "49")
      // Typed inline function
      check("let $f := function($x as xs:integer) as xs:integer { $x + 1 } return $f(10)", "11")
    }

    describe("Lexical closures (variable capturing)") {
      check(
        """
          |let $multiplier := 10,
          |    $times10 := function($x) { $x * $multiplier }
          |return
          |    $times10(5)
          |""".stripMargin,
        "50"
      )
      check(
        """
          |let $prefix := 'item: ',
          |    $makeItem := function($name) { $prefix || $name }
          |return
          |    $makeItem('widget')
          |""".stripMargin,
        "item: widget"
      )
      check(
        """
          |let $makeAdder := function($n) { function($x) { $x + $n } },
          |    $add5 := $makeAdder(5)
          |return
          |    $add5(20)
          |""".stripMargin,
        "25"
      )
    }

    describe("Named function references") {
      check("let $f := fn:abs#1 return $f(-42)", "42")
      check("let $f := fn:concat#2 return $f('foo', 'bar')", "foobar")
      check("let $f := fn:concat#3 return $f('a', 'b', 'c')", "abc")
      check("let $f := fn:string-length#1 return $f('Saxon')", "5")
      check("let $f := fn:upper-case#1 return $f('hello')", "HELLO")
      check("let $f := math:pow#2 return $f(2, 8)", "256")
      check("let $f := math:sqrt#1 return $f(16)", "4")
      // Direct invocation of named function reference
      check("(fn:concat#2)('ping', 'pong')", "pingpong")
    }

    describe("Partial function application (currying with `?`)") {
      check("let $greet := fn:concat('Hello, ', ?) return $greet('World!')", "Hello, World!")
      check("let $wrap := fn:concat('[', ?, ']') return $wrap('content')", "[content]")
      check("let $sub := fn:substring(?, 2, ?) return $sub('abcdef', 3)", "bcd")
      check("let $join := fn:string-join(?, ', ') return $join(('one', 'two', 'three'))", "one, two, three")
      // Currying an inline function
      check(
        """
          |let $f := function($a, $b, $c) { $a * 100 + $b * 10 + $c },
          |    $f1 := $f(1, ?, 3)
          |return
          |    $f1(2)
          |""".stripMargin,
        "123"
      )
      // Multiple placeholders
      check("let $f := fn:concat(?, ' - ', ?) return $f('Alpha', 'Omega')", "Alpha - Omega")
    }

    describe("fn:for-each") {
      check("string-join(fn:for-each(1 to 5, function($x) { $x * 2 }), ' ')", "2 4 6 8 10")
      check("string-join(fn:for-each(('apple', 'banana', 'cherry'), fn:upper-case#1), ' ')", "APPLE BANANA CHERRY")
      check("string-join(fn:for-each((), function($x) { $x * 2 }), ' ')", "")
    }

    describe("fn:filter") {
      check("string-join(for $x in fn:filter(1 to 10, function($n) { $n mod 2 = 0 }) return string($x), ' ')", "2 4 6 8 10")
      check("string-join(fn:filter(('a', 'ab', 'abc', 'd'), function($s) { string-length($s) > 1 }), ' ')", "ab abc")
      check("empty(fn:filter(1 to 5, function($n) { $n > 10 }))", "true")
    }

    describe("fn:fold-left and fn:fold-right") {
      check("fn:fold-left(1 to 5, 0, function($acc, $x) { $acc + $x })", "15")
      check("fn:fold-left(1 to 5, 1, function($acc, $x) { $acc * $x })", "120")
      check("fn:fold-left(('a', 'b', 'c'), 'start', function($acc, $x) { concat($acc, '.', $x) })", "start.a.b.c")
      check("fn:fold-right(('a', 'b', 'c'), 'end', function($x, $acc) { concat($x, '.', $acc) })", "a.b.c.end")
      check("fn:fold-left((), 42, function($acc, $x) { $acc + $x })", "42")
      check("fn:fold-right((), 42, function($x, $acc) { $acc + $x })", "42")
    }

    describe("fn:for-each-pair") {
      check("string-join(fn:for-each-pair(1 to 3, ('a', 'b', 'c'), function($n, $s) { $s || $n }), ' ')", "a1 b2 c3")
      // Truncation when sequences have unequal length
      check("string-join(fn:for-each-pair(1 to 5, ('a', 'b'), function($n, $s) { $s || $n }), ' ')", "a1 b2")
      check("string-join(fn:for-each-pair((1, 2), ('a', 'b', 'c', 'd'), function($n, $s) { $s || $n }), ' ')", "a1 b2")
    }

    describe("fn:sort with key extraction function") {
      check("string-join(for $x in fn:sort((5, 1, 4, 2, 3)) return string($x), ' ')", "1 2 3 4 5")
      check("string-join(fn:sort(('banana', 'fig', 'apple', 'date'), (), function($s) { string-length($s) }), ' ')", "fig date apple banana")
      check("string-join(for $x in fn:sort(1 to 5, (), function($n) { -$n }) return string($x), ' ')", "5 4 3 2 1")
    }

    describe("fn:apply") {
      check("fn:apply(fn:concat#3, ['foo', '-', 'bar'])", "foo-bar")
      check("fn:apply(math:pow#2, [2, 10])", "1024")
      check("fn:apply(function($a, $b) { $a * $b }, [6, 7])", "42")
    }

    describe("Maps and Arrays acting as functions in HOF contexts") {
      check("string-join(fn:for-each(('first', 'second'), map {'first': '1st', 'second': '2nd'}), ' ')", "1st 2nd")
      check("string-join(fn:for-each((1, 3), ['alpha', 'beta', 'gamma']), ' ')", "alpha gamma")
    }
  }
}
