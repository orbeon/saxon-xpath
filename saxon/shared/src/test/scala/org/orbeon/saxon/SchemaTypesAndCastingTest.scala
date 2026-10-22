package org.orbeon.saxon

class SchemaTypesAndCastingTest extends XPathTestBase {

  describe("XML Schema Types and Casting") {

    describe("Numeric Subtypes") {
      check("xs:integer('123456789012345678901234567890')", "123456789012345678901234567890")
      check("xs:long('9223372036854775807')", "9223372036854775807")
      check("xs:long('-9223372036854775808')", "-9223372036854775808")
      check("xs:int('2147483647')", "2147483647")
      check("xs:int('-2147483648')", "-2147483648")
      check("xs:short('32767')", "32767")
      check("xs:short('-32768')", "-32768")
      check("xs:byte('127')", "127")
      check("xs:byte('-128')", "-128")

      // Unsigned types
      check("xs:unsignedLong('18446744073709551615')", "18446744073709551615")
      check("xs:unsignedInt('4294967295')", "4294967295")
      check("xs:unsignedShort('65535')", "65535")
      check("xs:unsignedByte('255')", "255")
      check("xs:unsignedByte('0')", "0")

      // Sign-constrained integer types
      check("xs:positiveInteger('1')", "1")
      check("xs:nonNegativeInteger('0')", "0")
      check("xs:nonNegativeInteger('42')", "42")
      check("xs:negativeInteger('-1')", "-1")
      check("xs:nonPositiveInteger('0')", "0")
      check("xs:nonPositiveInteger('-42')", "-42")

      // Out of bounds checking
      checkError("xs:byte('128')", "FORG0001")
      checkError("xs:byte('-129')", "FORG0001")
      checkError("xs:unsignedByte('256')", "FORG0001")
      checkError("xs:unsignedByte('-1')", "FORG0001")
      checkError("xs:positiveInteger('0')", "FORG0001")
      checkError("xs:negativeInteger('0')", "FORG0001")
      checkError("xs:short('32768')", "FORG0001")
      checkError("xs:int('2147483648')", "FORG0001")
    }

    describe("xs:QName") {
      check("prefix-from-QName(QName('http://example.com/ns', 'p:my-elem'))", "p")
      check("local-name-from-QName(QName('http://example.com/ns', 'p:my-elem'))", "my-elem")
      check("namespace-uri-from-QName(QName('http://example.com/ns', 'p:my-elem'))", "http://example.com/ns")

      // QName with no prefix and no namespace
      check("empty(prefix-from-QName(QName('', 'simple')))", "true")
      check("local-name-from-QName(QName('', 'simple'))", "simple")
      check("namespace-uri-from-QName(QName('', 'simple'))", "")

      // QName equality
      check("QName('http://example.com', 'a:foo') = QName('http://example.com', 'b:foo')", "true")
      check("QName('http://example.com', 'a:foo') = QName('http://other.com', 'a:foo')", "false")
      check("QName('http://example.com', 'a:foo') = QName('http://example.com', 'a:bar')", "false")
    }

    describe("xs:anyURI") {
      check("string(xs:anyURI('http://example.com/test?a=1#frag'))", "http://example.com/test?a=1#frag")
      check("resolve-uri('sub/page.html', 'http://example.com/dir/')", "http://example.com/dir/sub/page.html")
      check("resolve-uri('/root.html', 'http://example.com/dir/')", "http://example.com/root.html")
      check("xs:anyURI('http://example.com') = xs:anyURI('http://example.com')", "true")
    }

    describe("xs:hexBinary and xs:base64Binary") {
      check("string(xs:hexBinary('0fb7'))", "0FB7") // canonical form is uppercase
      check("string(xs:hexBinary('A1B2C3D4'))", "A1B2C3D4")
      check("xs:hexBinary('0fb7') = xs:hexBinary('0FB7')", "true")

      check("string(xs:base64Binary('aGVsbG8='))", "aGVsbG8=")
      check("xs:base64Binary('aGVsbG8=') = xs:base64Binary('aGVsbG8=')", "true")

      // Cross-casting between hexBinary and base64Binary
      check("string(xs:hexBinary(xs:base64Binary('aGVsbG8=')))", "68656C6C6F")
      check("string(xs:base64Binary(xs:hexBinary('68656C6C6F')))", "aGVsbG8=")

      checkError("xs:hexBinary('0G')", "FORG0001") // invalid hex
      checkError("xs:base64Binary('==')", "FORG0001") // invalid base64
    }

    describe("xs:untypedAtomic and xs:boolean") {
      check("xs:untypedAtomic('42') + 8", "50")
      check("concat(xs:untypedAtomic('foo'), 'bar')", "foobar")
      check("xs:untypedAtomic('2023-11-05') = xs:date('2023-11-05')", "true")

      check("string(xs:boolean('true'))", "true")
      check("string(xs:boolean('false'))", "false")
      check("string(xs:boolean('1'))", "true")
      check("string(xs:boolean('0'))", "false")
      checkError("xs:boolean('yes')", "FORG0001")
      checkError("xs:boolean('no')", "FORG0001")
    }

    describe("cast as expressions") {
      check("'123' cast as xs:integer", "123")
      check("'3.14' cast as xs:double", "3.14")
      check("'3.14' cast as xs:decimal", "3.14")
      check("string('2023-11-05' cast as xs:date)", "2023-11-05")
      check("string('14:30:00' cast as xs:time)", "14:30:00")
      check("string('PT1H30M' cast as xs:dayTimeDuration)", "PT1H30M")
      check("123 cast as xs:string", "123")
      check("true() cast as xs:string", "true")

      // Empty sequence casting
      check("empty(() cast as xs:integer?)", "true")
      check("empty(() cast as xs:string?)", "true")
      check("empty(() cast as xs:date?)", "true")

      checkError("'abc' cast as xs:integer", "FORG0001")
      checkError("() cast as xs:integer", "XPTY0004")
    }

    describe("castable as expressions") {
      check("'123' castable as xs:integer", "true")
      check("'-456' castable as xs:integer", "true")
      check("'abc' castable as xs:integer", "false")
      check("'12.34' castable as xs:integer", "false")

      check("'3.14' castable as xs:double", "true")
      check("'INF' castable as xs:double", "true")
      check("'-INF' castable as xs:double", "true")
      check("'NaN' castable as xs:double", "true")
      check("'3.14e2' castable as xs:double", "true")

      check("'2023-11-05' castable as xs:date", "true")
      check("'2023-02-29' castable as xs:date", "false") // not a leap year
      check("'2024-02-29' castable as xs:date", "true")  // leap year
      check("'invalid' castable as xs:date", "false")

      check("'14:30:00' castable as xs:time", "true")
      check("'25:00:00' castable as xs:time", "false")

      check("'P1Y' castable as xs:yearMonthDuration", "true")
      check("'PT1H' castable as xs:yearMonthDuration", "false")
      check("'PT1H' castable as xs:dayTimeDuration", "true")

      // Empty sequence handling
      check("() castable as xs:integer", "false")
      check("() castable as xs:integer?", "true")
      check("() castable as xs:date?", "true")
    }

    describe("instance of expressions") {

      describe("Atomic types and subtyping") {
        check("42 instance of xs:integer", "true")
        check("42 instance of xs:decimal", "true") // integer extends decimal
        check("42 instance of xs:numeric", "true") // integer is numeric
        check("42 instance of xs:anyAtomicType", "true")
        check("42 instance of xs:string", "false")
        check("'hello' instance of xs:string", "true")
        check("'hello' instance of xs:anyAtomicType", "true")
        check("3.14 instance of xs:decimal", "true")
        check("3.14 instance of xs:integer", "false")
        check("xs:double('3.14') instance of xs:double", "true")
        check("xs:double('3.14') instance of xs:decimal", "false")

        // Numeric hierarchy
        check("xs:byte(10) instance of xs:byte", "true")
        check("xs:byte(10) instance of xs:short", "true")
        check("xs:byte(10) instance of xs:int", "true")
        check("xs:byte(10) instance of xs:long", "true")
        check("xs:byte(10) instance of xs:integer", "true")
        check("xs:byte(10) instance of xs:decimal", "true")

        check("xs:date('2023-11-05') instance of xs:date", "true")
        check("xs:date('2023-11-05') instance of xs:dateTime", "false")
        check("xs:dayTimeDuration('PT1H') instance of xs:duration", "true")
        check("xs:yearMonthDuration('P1Y') instance of xs:duration", "true")
      }

      describe("Occurrence indicators") {
        check("() instance of empty-sequence()", "true")
        check("() instance of xs:integer?", "true")
        check("() instance of xs:integer*", "true")
        check("() instance of xs:integer", "false")
        check("() instance of xs:integer+", "false")

        check("42 instance of xs:integer", "true")
        check("42 instance of xs:integer?", "true")
        check("42 instance of xs:integer*", "true")
        check("42 instance of xs:integer+", "true")
        check("42 instance of empty-sequence()", "false")

        check("(1, 2, 3) instance of xs:integer*", "true")
        check("(1, 2, 3) instance of xs:integer+", "true")
        check("(1, 2, 3) instance of xs:integer", "false")
        check("(1, 2, 3) instance of xs:integer?", "false")
      }

      describe("Item and complex types") {
        check("42 instance of item()", "true")
        check("(1, 2, 3) instance of item()*", "true")
        check("(1, 2, 3) instance of item()+", "true")
        check("(1, 2, 3) instance of item()", "false")

        // Maps, Arrays, Functions
        check("[1, 2, 3] instance of array(*)", "true")
        check("[1, 2, 3] instance of map(*)", "false")
        check("[1, 2, 3] instance of function(*)", "true") // arrays are functions in XPath 3.1!
        check("map {'a': 1} instance of map(*)", "true")
        check("map {'a': 1} instance of array(*)", "false")
        check("map {'a': 1} instance of function(*)", "true") // maps are functions in XPath 3.1!
        check("function($x) { $x + 1 } instance of function(*)", "true")
        check("function($x) { $x + 1 } instance of map(*)", "false")
      }
    }
  }
}
