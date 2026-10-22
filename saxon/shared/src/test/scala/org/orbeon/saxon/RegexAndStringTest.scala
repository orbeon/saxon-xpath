package org.orbeon.saxon

class RegexAndStringTest extends XPathTestBase {

  describe("Regular Expressions") {

    describe("fn:matches") {
      check("matches('abracadabra', 'bra')", "true")
      check("matches('abracadabra', '^a.*a$')", "true")
      check("matches('abracadabra', '^bra')", "false")
      check("matches('abracadabra', 'bra$')", "true")
      check("matches('abracadabra', 'cad$')", "false")

      // Flags
      check("matches('hello', 'HELLO')", "false")
      check("matches('hello', 'HELLO', 'i')", "true")
      check("matches(concat('line1', codepoints-to-string(10), 'line2'), '^line2')", "false")
      check("matches(concat('line1', codepoints-to-string(10), 'line2'), '^line2', 'm')", "true")
      check("matches(concat('line1', codepoints-to-string(10), 'line2'), 'line1.line2')", "false")
      check("matches(concat('line1', codepoints-to-string(10), 'line2'), 'line1.line2', 's')", "true")
      check("matches('hello', 'h e l l o', 'x')", "true")
      check("matches('a+b?c*', 'a+b?c*', 'q')", "true")
      check("matches('a+b?c*', 'A+B?C*', 'qi')", "true")

      // Character classes & Unicode
      check("matches('12345', '^\\d+$')", "true")
      check("matches('123a45', '^\\d+$')", "false")
      check("matches('word123', '^\\w+$')", "true")
      check("matches('word_123', '^\\w+$')", "false") // in XPath \w does not include '_' (\p{P})
      check("matches('hello world', '^\\S+\\s+\\S+$')", "true")
      check("matches('Grüß Gott', '^\\p{L}+\\s+\\p{L}+$')", "true")

      // Edge cases
      check("matches('', '^$')", "true")
      check("matches('', 'a*')", "true")
      check("matches('', 'a+')", "false")
      check("matches((), 'abc')", "false")
    }

    describe("fn:replace") {
      check("replace('abracadabra', 'bra', '*')", "a*cada*")
      check("replace('abracadabra', 'a(.)', 'a*')", "a*ra*a*a*ra")
      check("replace('hello', 'L', 'x', 'i')", "hexxo")
      check("replace('a.b.c', '.', '-', 'q')", "a-b-c")

      // Backreferences
      check("replace('abcd', '(ab)(cd)', '$2$1')", "cdab")
      check("replace('123-456-789', '(\\d+)-(\\d+)-(\\d+)', '$3.$2.$1')", "789.456.123")
      check("replace('hello', '(.)', '$1$1')", "hheelllloo")

      // Escaped characters in replacement
      check("replace('item', 'item', '\\$10')", "$10")
      check("replace('item', 'item', '\\\\')", "\\")
      check("replace('item', 'item', 'a\\$b')", "a$b")
      check("replace('item', 'item', 'a\\\\b')", "a\\b")

      // Edge cases
      check("replace((), 'a', 'b')", "")
    }

    describe("fn:tokenize") {
      // 1-argument tokenize (XPath 3.1 whitespace split)
      checkStrings("tokenize('The cat sat on the mat')", List("The", "cat", "sat", "on", "the", "mat"))
      checkStrings("tokenize('   leading and   trailing   ')", List("leading", "and", "trailing"))
      checkStrings("tokenize('')", Nil)
      checkStrings("tokenize('   ')", Nil)

      // 2-argument tokenize
      checkStrings("tokenize('red, green, blue', ',\\s*')", List("red", "green", "blue"))
      checkStrings("tokenize('1-2-3-4-5', '-')", List("1", "2", "3", "4", "5"))
      checkStrings("tokenize(';a;b;', ';')", List("", "a", "b", ""))

      // 3-argument tokenize with flags
      checkStrings("tokenize('aXbXc', 'x', 'i')", List("a", "b", "c"))

      // Edge cases
      checkStrings("tokenize((), ',')", Nil)
    }

    describe("fn:analyze-string") {
      check(
        """analyze-string('2023-11-04', '(\d{4})-(\d{2})-(\d{2})')/fn:match/fn:group[@nr=1]/string()""",
        "2023"
      )
      check(
        """analyze-string('2023-11-04', '(\d{4})-(\d{2})-(\d{2})')/fn:match/fn:group[@nr=2]/string()""",
        "11"
      )
      check(
        """analyze-string('2023-11-04', '(\d{4})-(\d{2})-(\d{2})')/fn:match/fn:group[@nr=3]/string()""",
        "04"
      )
      checkStrings(
        """analyze-string('abc123def456', '\d+')/fn:match/string()""",
        List("123", "456")
      )
      checkStrings(
        """analyze-string('abc123def456', '\d+')/fn:non-match/string()""",
        List("abc", "def")
      )
      check(
        """count(analyze-string('', 'a+')/*)""",
        "0"
      )
    }
  }

  describe("String Library") {

    describe("Unicode Codepoints") {
      checkStrings("string-to-codepoints('Thérèse')", List("84", "104", "233", "114", "232", "115", "101"))
      check("codepoints-to-string((84, 104, 233, 114, 232, 115, 101))", "Thérèse")
      checkStrings("string-to-codepoints('')", Nil)
      check("codepoints-to-string(())", "")
      check("codepoints-to-string(65)", "A")
    }

    describe("String concatenation and join") {
      check("concat('a', 'b', 'c', 'd')", "abcd")
      check("'hello' || ' ' || 'world'", "hello world")
      check("string-join(('a', 'b', 'c'))", "abc")
      check("string-join(('a', 'b', 'c'), '-')", "a-b-c")
      check("string-join((), '-')", "")
      check("string-join('single', '-')", "single")
    }

    describe("fn:substring") {
      check("substring('12345', 2)", "2345")
      check("substring('12345', 2, 3)", "234")
      check("substring('12345', 1.5, 2.6)", "234")
      check("substring('12345', 0, 3)", "12")
      check("substring('12345', -1, 3)", "1")
      check("substring('12345', 6, 2)", "")
      check("substring('12345', 2, 0)", "")
      check("substring('', 1, 2)", "")
      check("substring((), 1, 2)", "")
    }

    describe("fn:string-length") {
      check("string-length('hello')", "5")
      check("string-length('')", "0")
      check("string-length(())", "0")
      check("string-length('Grüß Gott')", "9")
    }

    describe("fn:normalize-space") {
      check("normalize-space('   foo   bar   baz   ')", "foo bar baz")
      check("normalize-space('')", "")
      check("normalize-space(())", "")
      check("normalize-space(concat(codepoints-to-string(9), codepoints-to-string(10), 'foo', codepoints-to-string(13), ' ', 'bar', ' '))", "foo bar")
    }

    describe("Case conversions") {
      check("upper-case('hello world!')", "HELLO WORLD!")
      check("upper-case('Grüß Gott')", "GRÜSS GOTT")
      check("lower-case('HELLO WORLD!')", "hello world!")
      check("upper-case('')", "")
      check("upper-case(())", "")
      check("lower-case(())", "")
    }

    describe("fn:translate") {
      check("translate('bar', 'abc', 'ABC')", "BAr")
      check("translate('--aaa--', 'abc-', 'ABC')", "AAA")
      check("translate('abcd', 'bc', 'CB')", "aCBd")
      check("translate('abcd', 'abcd', '')", "")
      check("translate('', 'a', 'b')", "")
      check("translate((), 'a', 'b')", "")
    }

    describe("fn:contains, fn:starts-with, fn:ends-with") {
      check("contains('tattoo', 'tat')", "true")
      check("contains('tattoo', 'too')", "true")
      check("contains('tattoo', 'att')", "true")
      check("contains('tattoo', 'ttt')", "false")
      check("contains('tattoo', '')", "true")
      check("contains('', '')", "true")
      check("contains('', 'a')", "false")
      check("contains((), 'a')", "false")

      check("starts-with('tattoo', 'tat')", "true")
      check("starts-with('tattoo', 'too')", "false")
      check("starts-with('tattoo', '')", "true")
      check("starts-with((), 'a')", "false")

      check("ends-with('tattoo', 'too')", "true")
      check("ends-with('tattoo', 'tat')", "false")
      check("ends-with('tattoo', '')", "true")
      check("ends-with((), 'a')", "false")
    }

    describe("fn:substring-before and fn:substring-after") {
      check("substring-before('1999/04/01', '/')", "1999")
      check("substring-after('1999/04/01', '/')", "04/01")
      check("substring-before('1999/04/01', '?')", "")
      check("substring-after('1999/04/01', '?')", "")
      check("substring-before('tattoo', 'tattoo')", "")
      check("substring-after('tattoo', 'tattoo')", "")
      check("substring-before('tattoo', '')", "")
      check("substring-after('tattoo', '')", "tattoo")
      check("substring-before((), '/')", "")
      check("substring-after((), '/')", "")
    }

    describe("URI encoding and escaping") {
      check("encode-for-uri('http://example.com/test?a=1&b=2')", "http%3A%2F%2Fexample.com%2Ftest%3Fa%3D1%26b%3D2")
      check("encode-for-uri('~bébé')", "~b%C3%A9b%C3%A9")
      check("iri-to-uri('http://www.example.com/~bébé')", "http://www.example.com/~b%C3%A9b%C3%A9")
      check("escape-html-uri('http://example.com/test?a=1&b=2')", "http://example.com/test?a=1&b=2")
    }
  }
}
