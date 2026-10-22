package org.orbeon.saxon

class MapAndArrayTest extends XPathTestBase {

  describe("XPath 3.1 Maps") {

    describe("Map constructors and basic access") {
      check("map:size(map {})", "0")
      check("map:size(map { 'a': 1, 'b': 2 })", "2")
      check("map:size(map { 'a': 1, 'b': 2, 'c': (3, 4, 5) })", "3")
      check("map:get(map { 'a': 10, 'b': 20 }, 'a')", "10")
      check("map:get(map { 'a': 10, 'b': 20 }, 'b')", "20")
      check("empty(map:get(map { 'a': 10 }, 'c'))", "true")
      check("map:contains(map { 'a': 10 }, 'a')", "true")
      check("map:contains(map { 'a': 10 }, 'b')", "false")
      check("string-join(sort(map:keys(map { 'x': 1, 'y': 2, 'z': 3 })), ' ')", "x y z")
    }

    describe("Map modification and immutability") {
      check("let $m := map { 'a': 1 }, $m2 := map:put($m, 'b', 2) return map:size($m) || ' ' || map:size($m2)", "1 2")
      check("let $m := map { 'a': 1 }, $m2 := map:put($m, 'a', 99) return $m2?a || ' ' || $m?a", "99 1")
      check("let $m := map:entry('foo', 'bar') return map:size($m) || ' ' || $m?foo", "1 bar")
      check("let $m := map { 'a': 1, 'b': 2, 'c': 3 } return map:size(map:remove($m, 'b'))", "2")
      check("let $m := map { 'a': 1, 'b': 2, 'c': 3 } return map:contains(map:remove($m, 'b'), 'b')", "false")
      check("let $m := map { 'a': 1, 'b': 2, 'c': 3 } return map:size(map:remove($m, ('a', 'c')))", "1")
      check("let $m := map { 'a': 1 } return map:size(map:remove($m, 'non-existent'))", "1")
    }

    describe("Map merge and duplicate strategies") {
      check("map:size(map:merge(()))", "0")
      check("map:size(map:merge((map {'a': 1}, map {'b': 2})))", "2")
      // Default duplicate handling is use-first
      check("let $m := map:merge((map {'k': 1}, map {'k': 2})) return $m?k", "1")
      check("let $m := map:merge((map {'k': 1}, map {'k': 2}), map {'duplicates': 'use-first'}) return $m?k", "1")
      check("let $m := map:merge((map {'k': 1}, map {'k': 2}), map {'duplicates': 'use-last'}) return $m?k", "2")
      check("let $m := map:merge((map {'k': 1}, map {'k': 2}), map {'duplicates': 'combine'}) return string-join(for $v in $m?k return string($v), ',')", "1,2")
    }

    describe("Map higher-order and utility functions") {
      check(
        """
          |string-join(
          |  sort(map:for-each(map {'a': 10, 'b': 20}, function($k, $v) { $k || '=' || $v })),
          |  ', '
          |)
          |""".stripMargin,
        "a=10, b=20"
      )
      check("map:find(map {'a': [map {'b': 42}]}, 'b')?1", "42")
    }

    describe("Map as a function") {
      check("let $m := map { 'greeting': 'hello', 'target': 'world' } return $m('greeting') || ' ' || $m('target')", "hello world")
      check("empty((map { 'greeting': 'hello' })('unknown'))", "true")
    }
  }

  describe("XPath 3.1 Arrays") {

    describe("Array constructors and basic access") {
      check("array:size([])", "0")
      check("array:size([1, 2, 3])", "3")
      check("array:size([1, (2, 3), 4])", "3") // each comma-separated expr is one member
      check("array:size(array { 1 to 5 })", "5") // curly array constructor creates 1 member per item
      check("array:size(array { () })", "0")
      check("array:get([10, 20, 30], 1)", "10")
      check("array:get([10, 20, 30], 2)", "20")
      check("array:get([10, 20, 30], 3)", "30")
      check("string-join(for $i in array:get([1, (2, 3), 4], 2) return string($i), ',')", "2,3")
    }

    describe("Array modification and immutability") {
      check("let $a := [1, 2], $a2 := array:append($a, 3) return array:size($a) || ' ' || array:size($a2)", "2 3")
      check("let $a := [1, 2, 3] return array:get(array:put($a, 2, 99), 2) || ' ' || array:get($a, 2)", "99 2")
      check("let $a := array:insert-before([1, 3], 2, 2) return string-join(array:flatten($a), ' ')", "1 2 3")
      check("let $a := array:insert-before([2, 3], 1, 1) return string-join(array:flatten($a), ' ')", "1 2 3")
      check("let $a := array:remove([1, 2, 3, 4], 2) return string-join(array:flatten($a), ' ')", "1 3 4")
      check("let $a := array:remove([1, 2, 3, 4], (1, 4)) return string-join(array:flatten($a), ' ')", "2 3")
      check("let $a := array:subarray([10, 20, 30, 40, 50], 2, 3) return string-join(array:flatten($a), ' ')", "20 30 40")
    }

    describe("Array structural transformations") {
      check("array:head([10, 20, 30])", "10")
      check("string-join(array:flatten(array:tail([10, 20, 30])), ' ')", "20 30")
      check("array:size(array:tail([10]))", "0")
      check("string-join(array:flatten(array:reverse([1, 2, 3])), ' ')", "3 2 1")
      check("string-join(array:flatten(array:join(([1, 2], [3, 4], [5]))), ' ')", "1 2 3 4 5")
      check("string-join(array:flatten([[1, 2], [3, [4, 5]]]), ' ')", "1 2 3 4 5")
    }

    describe("Array higher-order functions") {
      check("string-join(array:flatten(array:for-each([1, 2, 3], function($x) { $x * 10 })), ' ')", "10 20 30")
      check("string-join(array:flatten(array:filter([1, 2, 3, 4, 5], function($x) { $x mod 2 = 1 })), ' ')", "1 3 5")
      check("array:fold-left([1, 2, 3, 4], 0, function($acc, $x) { $acc + $x })", "10")
      check("array:fold-left(['a', 'b', 'c'], '', function($acc, $x) { concat($acc, $x) })", "abc")
      check("array:fold-right(['a', 'b', 'c'], '', function($x, $acc) { concat($acc, $x) })", "cba")
      check("string-join(array:flatten(array:sort([3, 1, 4, 1, 5, 9])), ' ')", "1 1 3 4 5 9")
    }

    describe("Array as a function") {
      check("let $a := ['first', 'second', 'third'] return $a(1) || ' ' || $a(3)", "first third")
    }

    describe("Deep equality with Maps and Arrays") {
      check("fn:deep-equal(map {'a': 1, 'b': 2}, map {'b': 2, 'a': 1})", "true")
      check("fn:deep-equal(map {'a': 1, 'b': 2}, map {'a': 1, 'b': 3})", "false")
      check("fn:deep-equal([1, 2, [3, 4]], [1, 2, [3, 4]])", "true")
      check("fn:deep-equal([1, 2, [3, 4]], [1, 2, [3, 5]])", "false")
      check("fn:deep-equal(map {'data': [1, 2, map {'x': 10}]}, map {'data': [1, 2, map {'x': 10}]})", "true")
    }
  }
}
