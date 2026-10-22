package org.orbeon.saxon

class MathAndNumberTest extends XPathTestBase {

  describe("Math Library (math:*)") {

    describe("Constants and Powers") {
      check("math:pi() > 3.14159 and math:pi() < 3.14160", "true")
      check("math:exp(0)", "1")
      check("round(math:exp(1) * 1000) div 1000", "2.718")
      check("math:exp10(0)", "1")
      check("math:exp10(2)", "100")
      check("math:exp10(3)", "1000")
      check("math:log(1)", "0")
      check("round(math:log(math:exp(5)))", "5")
      check("math:log10(1)", "0")
      check("math:log10(100)", "2")
      check("math:log10(1000)", "3")
      check("math:pow(2, 3)", "8")
      check("math:pow(2, 10)", "1024")
      check("math:pow(10, -2)", "0.01")
      check("math:pow(9, 0.5)", "3")
      check("math:sqrt(16)", "4")
      check("math:sqrt(0)", "0")
      check("math:sqrt(2) > 1.414 and math:sqrt(2) < 1.415", "true")
    }

    describe("Trigonometric functions") {
      check("math:sin(0)", "0")
      check("round(math:sin(math:pi() div 2))", "1")
      check("math:cos(0)", "1")
      check("round(math:cos(math:pi()))", "-1")
      check("round(math:cos(math:pi() div 2))", "0")
      check("math:tan(0)", "0")
      check("round(math:tan(math:pi() div 4) * 1000) div 1000", "1")
      check("math:asin(0)", "0")
      check("round(math:asin(1) div (math:pi() div 2))", "1")
      check("math:acos(1)", "0")
      check("round(math:acos(0) div (math:pi() div 2))", "1")
      check("math:atan(0)", "0")
      check("round(math:atan(1) div (math:pi() div 4))", "1")
      check("math:atan2(0, 1)", "0")
      check("round(math:atan2(1, 0) div (math:pi() div 2))", "1")
    }

    describe("Math edge cases") {
      check("string(math:sqrt(-1))", "NaN")
      check("string(math:log(-1))", "NaN")
      check("string(math:log(0))", "-INF")
      check("empty(math:sqrt(()))", "true")
      check("empty(math:sin(()))", "true")
      check("empty(math:cos(()))", "true")
      check("empty(math:exp(()))", "true")
      check("empty(math:log(()))", "true")
      check("empty(math:pow((), 2))", "true")
    }
  }

  describe("Core Number Functions") {

    describe("fn:abs") {
      check("abs(5)", "5")
      check("abs(-5)", "5")
      check("abs(0)", "0")
      check("abs(-3.14)", "3.14")
      check("abs(xs:decimal('-10.5'))", "10.5")
      check("empty(abs(()))", "true")
    }

    describe("fn:ceiling and fn:floor") {
      check("ceiling(10.1)", "11")
      check("ceiling(10.9)", "11")
      check("ceiling(-10.1)", "-10")
      check("ceiling(-10.9)", "-10")
      check("ceiling(5)", "5")
      check("empty(ceiling(()))", "true")

      check("floor(10.1)", "10")
      check("floor(10.9)", "10")
      check("floor(-10.1)", "-11")
      check("floor(-10.9)", "-11")
      check("floor(5)", "5")
      check("empty(floor(()))", "true")
    }

    describe("fn:round (1 and 2 arguments)") {
      // 1 argument
      check("round(2.5)", "3")
      check("round(2.49)", "2")
      check("round(-2.5)", "-2")
      check("round(-2.51)", "-3")
      check("round(0)", "0")
      check("empty(round(()))", "true")

      // 2 arguments (precision)
      check("round(2.475, 2)", "2.48")
      check("round(2.474, 2)", "2.47")
      check("round(123.456, 0)", "123")
      check("round(125, -1)", "130")
      check("round(124, -1)", "120")
      check("round(650, -2)", "700")
      check("empty(round((), 2))", "true")
    }

    describe("fn:round-half-to-even") {
      check("round-half-to-even(2.5)", "2")
      check("round-half-to-even(3.5)", "4")
      check("round-half-to-even(-2.5)", "-2")
      check("round-half-to-even(-3.5)", "-4")
      check("round-half-to-even(2.475, 2)", "2.48")
      check("round-half-to-even(2.485, 2)", "2.48")
      check("round-half-to-even(2.495, 2)", "2.5")
      check("empty(round-half-to-even(()))", "true")
      check("empty(round-half-to-even((), 2))", "true")
    }

    describe("fn:number") {
      check("number('42')", "42")
      check("number('  3.14  ')", "3.14")
      check("number('+100')", "100")
      check("number('-50')", "-50")
      check("number('1e3')", "1000")
      check("string(number('abc'))", "NaN")
      check("string(number(''))", "NaN")
      check("string(number(()))", "NaN")
      check("number(true())", "1")
      check("number(false())", "0")
    }
  }

  describe("fn:format-integer") {
    check("format-integer(42, '1')", "42")
    check("format-integer(5, '001')", "005")
    check("format-integer(123, '00000')", "00123")
    check("format-integer(42, 'w')", "forty two")
    check("format-integer(42, 'W')", "FORTY TWO")
    check("format-integer(42, 'Ww')", "Forty Two")
    check("format-integer(1, 'w')", "one")
    check("format-integer(14, 'w')", "fourteen")
    check("format-integer(100, 'w')", "one hundred")
    check("format-integer(42, 'i')", "xlii")
    check("format-integer(42, 'I')", "XLII")
    check("format-integer(1999, 'I')", "MCMXCIX")
    check("format-integer(2024, 'I')", "MMXXIV")
    check("format-integer(1, 'a')", "a")
    check("format-integer(26, 'a')", "z")
    check("format-integer(27, 'a')", "aa")
    check("format-integer(1, 'A')", "A")
    check("format-integer(26, 'A')", "Z")
    check("format-integer((), '1')", "")
  }

  describe("Arithmetic and Special Values (NaN, INF)") {
    check("string(1 div xs:double(0.0))", "INF")
    check("string(-1 div xs:double(0.0))", "-INF")
    check("string(xs:double(0.0) div xs:double(0.0))", "NaN")
    check("xs:double('NaN') = xs:double('NaN')", "false")
    check("xs:double('NaN') != xs:double('NaN')", "true")
    check("xs:double('INF') > 999999999", "true")
    check("xs:double('-INF') < -999999999", "true")
    check("xs:double('INF') = xs:double('INF')", "true")
    check("xs:double('INF') != xs:double('-INF')", "true")

    // Promotions and mixed types
    check("xs:integer(5) + xs:decimal(2.5)", "7.5")
    check("xs:decimal(2.5) + xs:double(1.5)", "4")
    check("xs:integer(10) * xs:double(2.5)", "25")
    check("xs:decimal(7.5) div xs:integer(3)", "2.5")
    check("10 idiv 3", "3")
    check("-10 idiv 3", "-3")
    check("10 mod 3", "1")
    check("-10 mod 3", "-1")
    check("10.5 mod 3", "1.5")
  }
}
