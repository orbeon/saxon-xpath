package org.orbeon.saxon

class ArrowAndLookupTest extends XPathTestBase {

  describe("Arrow operator (=>)") {

    describe("Basic single function invocation") {
      check("'  hello  ' => normalize-space()", "hello")
      check("'hello' => upper-case()", "HELLO")
      check("'HELLO' => lower-case()", "hello")
      check("'hello' => string-length()", "5")

      check("'hello world' => substring(1, 5)", "hello")
      check("'hello world' => substring-after(' ')", "world")
      check("'hello world' => substring-before(' ')", "hello")
      check("'banana' => replace('a', 'o')", "bonono")
      check("'banana' => translate('a', 'o')", "bonono")
      checkStrings("'a,b,c' => tokenize(',')", List("a", "b", "c"))

      check("('a', 'b', 'c') => string-join('-')", "a-b-c")
      check("(1, 2, 3, 4, 5) => count()", "5")
      check("(1, 2, 3, 4, 5) => sum()", "15")
      check("(10, 20, 30) => avg()", "20")
      check("(3, 1, 4, 1, 5, 9) => min()", "1")
      check("(3, 1, 4, 1, 5, 9) => max()", "9")
      checkStrings("('c', 'a', 'b') => sort()", List("a", "b", "c"))
      checkStrings("(1, 2, 3, 4, 5) => reverse()", List("5", "4", "3", "2", "1"))

      check("() => count()", "0")
      check("() => empty()", "true")
      check("() => exists()", "false")
      check("() => string-join(',')", "")
    }

    describe("Arrow operator chaining") {
      check("'  hello world  ' => normalize-space() => upper-case()", "HELLO WORLD")
      check("'  hello world  ' => normalize-space() => upper-case() => substring(1, 5)", "HELLO")
      check("'123-456-789' => tokenize('-') => reverse() => string-join('/')", "789/456/123")
      check("'1, 2, 3, 4, 5' => tokenize(',') => for-each(normalize-space#1) => for-each(xs:integer#1) => sum()", "15")
      check("(5, 10, 15) => avg() => xs:integer() => string()", "10")
    }

    describe("Arrow operator with Higher-Order Functions") {
      checkStrings("(1 to 5) => for-each(function($x) { $x * 2 })", List("2", "4", "6", "8", "10"))
      checkStrings("(1 to 10) => filter(function($x) { $x mod 2 = 0 })", List("2", "4", "6", "8", "10"))
      check("(1 to 5) => fold-left(0, function($acc, $x) { $acc + $x })", "15")
      check("(1 to 5) => fold-left(1, function($acc, $x) { $acc * $x })", "120")
      check("('a', 'b', 'c') => fold-right('', concat#2)", "abc")
      check(
        """(1 to 10)
          |  => filter(function($x) { $x mod 2 = 1 })
          |  => for-each(function($x) { $x * $x })
          |  => sum()""".stripMargin,
        "165"
      )
    }

    describe("Arrow operator with dynamic function references and variables") {
      check("let $f := upper-case#1 return 'hello' => $f()", "HELLO")
      check("let $add := function($a, $b) { $a + $b } return 10 => $add(5)", "15")
      check("'hello' => (function($x) { concat($x, ' world') })()", "hello world")
      check(
        """let $funcs := [upper-case#1, lower-case#1]
          |return 'Hello' => ($funcs?1)() => ($funcs?2)()""".stripMargin,
        "hello"
      )
      check(
        """let $ops := map {
          |  'double': function($x) { $x * 2 },
          |  'inc':    function($x) { $x + 1 }
          |}
          |return 5 => ($ops?double)() => ($ops?inc)()""".stripMargin,
        "11"
      )
    }

    describe("Arrow operator with Map and Array module functions") {
      check("[1, 2, 3] => array:size()", "3")
      check("[10, 20, 30] => array:get(2)", "20")
      check("[1, 2, 3] => array:append(4) => array:size()", "4")
      check("[1, 2, 3] => array:put(2, 99) => array:get(2)", "99")
      check("[10, 20, 30, 40] => array:subarray(2, 2) => array:size()", "2")
      check("[1, 2, 3] => array:reverse() => array:get(1)", "3")
      check("[1, 2, 3] => array:for-each(function($x) { $x * 10 }) => array:get(3)", "30")

      check("map { 'a': 1, 'b': 2 } => map:size()", "2")
      check("map { 'a': 1, 'b': 2 } => map:contains('a')", "true")
      check("map { 'a': 1, 'b': 2 } => map:get('b')", "2")
      check("map { 'a': 1 } => map:put('b', 2) => map:size()", "2")
      check("map { 'a': 1, 'b': 2 } => map:remove('a') => map:contains('a')", "false")
      checkStrings("map { 'b': 2, 'a': 1 } => map:keys() => sort()", List("a", "b"))
    }
  }

  describe("Lookup operator (?)") {

    describe("Map lookup with NCName and String Literal") {
      check("let $m := map {'name': 'Alice', 'age': 30} return $m?name", "Alice")
      check("let $m := map {'name': 'Alice', 'age': 30} return $m?age", "30")
      check("let $m := map {'name': 'Alice'} return empty($m?missing)", "true")
      check("let $m := map {'first-name': 'Alice'} return $m?('first-name')", "Alice")
      check("let $m := map {'name': 'Alice'} return $m?('name')", "Alice")
      checkStrings("let $m := map {'a': 1, 'b': 2, 'c': 3} return $m?('a', 'c')", List("1", "3"))
      check("let $m := map {1: 'one', 2: 'two'} return $m?1", "one")
      check("let $m := map {1: 'one', 2: 'two'} return $m?2", "two")
      check("let $m := map {1: 'one', 2: 'two'} return empty($m?3)", "true")
      check("let $m := map {'a': 1} return $m?*", "1")
      checkStrings("let $m := map {'a': 1, 'b': 2} return sort($m?*)", List("1", "2"))
    }

    describe("Array lookup with Integer and Wildcard") {
      check("['a', 'b', 'c']?1", "a")
      check("['a', 'b', 'c']?2", "b")
      check("['a', 'b', 'c']?3", "c")
      check("let $a := ['a', 'b', 'c'], $i := 2 return $a?($i)", "b")
      check("['a', 'b', 'c']?(1 + 1)", "b")
      checkStrings("['a', 'b', 'c', 'd']?(1, 3)", List("a", "c"))
      checkStrings("['a', 'b', 'c', 'd']?(2 to 4)", List("b", "c", "d"))
      checkStrings("[10, 20, 30]?*", List("10", "20", "30"))
      checkStrings("[(1, 2), (3, 4)]?*", List("1", "2", "3", "4"))
      checkStrings("['a', ('b', 'c'), 'd']?*", List("a", "b", "c", "d"))
    }

    describe("Lookup on sequence of maps and arrays") {
      checkStrings(
        """let $people := (
          |  map {'name': 'Alice', 'dept': 'Dev'},
          |  map {'name': 'Bob',   'dept': 'QA'},
          |  map {'name': 'Carol', 'dept': 'Dev'}
          |)
          |return $people?name""".stripMargin,
        List("Alice", "Bob", "Carol")
      )
      checkStrings(
        """let $arrays := ([1, 2], [3, 4], [5, 6])
          |return $arrays?1""".stripMargin,
        List("1", "3", "5")
      )
      checkStrings(
        """let $arrays := ([1, 2], [3, 4])
          |return $arrays?*""".stripMargin,
        List("1", "2", "3", "4")
      )
      checkStrings(
        """let $items := (
          |  map {'x': 10, 'y': 20},
          |  map {'y': 30},
          |  map {'x': 40}
          |)
          |return $items?x""".stripMargin,
        List("10", "40")
      )
    }

    describe("Lookup on empty sequence") {
      check("empty(()?name)", "true")
      check("empty(()?1)", "true")
      check("empty(()?*)", "true")
      check("empty(()?('a', 'b'))", "true")
    }

    describe("Unary lookup") {
      checkStrings(
        """let $people := (
          |  map {'name': 'Alice', 'role': 'admin'},
          |  map {'name': 'Bob',   'role': 'user'},
          |  map {'name': 'Carol', 'role': 'admin'}
          |)
          |return $people[?role = 'admin']?name""".stripMargin,
        List("Alice", "Carol")
      )
      checkStrings(
        """let $tuples := ([1, 'one'], [2, 'two'], [3, 'three'])
          |return $tuples[?1 > 1]?2""".stripMargin,
        List("two", "three")
      )
      check(
        """let $arrays := ([1, 2, 3], [4, 5, 6], [1, 1, 1])
          |return $arrays[?* = 5]?1""".stripMargin,
        "4"
      )
    }

    describe("Chained and Nested Lookups") {
      check(
        """let $data := map {
          |  'user': map {
          |    'name': 'Alice',
          |    'address': map {
          |      'city': 'Portland'
          |    }
          |  }
          |}
          |return $data?user?address?city""".stripMargin,
        "Portland"
      )
      check("[[1, 2], [3, 4]]?1?2", "2")
      check("[[1, 2], [3, 4]]?2?1", "3")
      check("[[[42]]]?1?1?1", "42")
      checkStrings(
        """let $doc := map {
          |  'store': map {
          |    'books': [
          |      map {'title': 'Book 1', 'price': 10},
          |      map {'title': 'Book 2', 'price': 25},
          |      map {'title': 'Book 3', 'price': 15}
          |    ]
          |  }
          |}
          |return $doc?store?books?*?title""".stripMargin,
        List("Book 1", "Book 2", "Book 3")
      )
      checkStrings(
        """let $doc := map {
          |  'books': [
          |    map {'title': 'A', 'price': 10},
          |    map {'title': 'B', 'price': 25},
          |    map {'title': 'C', 'price': 15}
          |  ]
          |}
          |return $doc?books?*[?price > 12]?title""".stripMargin,
        List("B", "C")
      )
      checkStrings(
        """let $catalog := [
          |  map {'category': 'tech', 'tags': ['scala', 'xpath']},
          |  map {'category': 'art',  'tags': ['painting', 'drawing']}
          |]
          |return $catalog?*[?category = 'tech']?tags?*""".stripMargin,
        List("scala", "xpath")
      )
    }

    describe("Combining Arrow and Lookup operators") {
      check(
        """let $doc := map {'greeting': '  hello world  '}
          |return $doc?greeting => normalize-space() => upper-case()""".stripMargin,
        "HELLO WORLD"
      )
      check(
        """let $m := map {'numbers': [1, 2, 3, 4, 5]}
          |return $m?numbers => array:size()""".stripMargin,
        "5"
      )
      check(
        """let $m := map {'numbers': [1, 2, 3, 4, 5]}
          |return $m?numbers?* => sum()""".stripMargin,
        "15"
      )
      check(
        """(map {'a': 10} => map:put('b', 20))?b""",
        "20"
      )
      check(
        """([1, 2, 3] => array:append(4))?4""",
        "4"
      )
    }
  }
}
