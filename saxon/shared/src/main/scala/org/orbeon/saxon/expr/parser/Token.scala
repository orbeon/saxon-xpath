////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class holds static constants and methods defining the lexical tokens used in
  * XPath and XQuery, and associated keywords.
  */

package org.orbeon.saxon.expr.parser

import java.util.HashMap


object Token {

  /**
    * Pseudo-token representing implicit end of expression (the parser doesn't care what follows
    * the expression)
    */
  val IMPLICIT_EOF: Int = -1

  /**
    * Pseudo-token representing the end of the expression
    */
  val EOF: Int = 0

  /**
    * "union" or "|" token
    */
  val UNION: Int = 1

  /**
    * Forwards "/"
    */
  val SLASH: Int = 2

  /**
    * At token, "@"
    */
  val AT: Int = 3

  /**
    * Left square bracket
    */
  val LSQB: Int = 4

  /**
    * Left parenthesis
    */
  val LPAR: Int = 5

  /**
    * Equals token ("=")
    */
  val EQUALS: Int = 6

  /**
    * Comma token
    */
  val COMMA: Int = 7

  /**
    * Double forwards slash, "//"
    */
  val SLASH_SLASH: Int = 8

  /**
    * Operator "or"
    */
  val OR: Int = 9

  /**
    * Operator "and"
    */
  val AND: Int = 10

  /**
    * Operator "&gt;"
    */
  val GT: Int = 11

  /**
    * Operator "&lt;"
    */
  val LT: Int = 12

  /**
    * Operator "&gt;="
    */
  val GE: Int = 13

  /**
    * Operator "&lt;="
    */
  val LE: Int = 14

  /**
    * Operator "+"
    */
  val PLUS: Int = 15

  /**
    * Binary minus operator
    */
  val MINUS: Int = 16

  /**
    * Multiply operator, "*" when used in an operator context
    */
  val MULT: Int = 17

  /**
    * Operator "div"
    */
  val DIV: Int = 18

  /**
    * Operator "mod"
    */
  val MOD: Int = 19

  /**
    * Operator "is"
    */
  val IS: Int = 20

  /**
    * "$" symbol
    */
  val DOLLAR: Int = 21

  /**
    * Operator not-equals. That is, "!="
    */
  val NE: Int = 22

  /**
    * Operator "intersect"
    */
  val INTERSECT: Int = 23

  /**
    * Operator "except"
    */
  val EXCEPT: Int = 24

  /**
    * Keyword "return"
    */
  val RETURN: Int = 25

  /**
    * Ketword "then"
    */
  val THEN: Int = 26

  /**
    * Keyword "else"
    */
  val ELSE: Int = 27

  /**
    * Keyword "where"
    */
  val WHERE: Int = 28

  /**
    * Operator "to"
    */
  val TO: Int = 29

  /**
    * Operator "||"
    */
  val CONCAT: Int = 30

  /**
    * Keyword "in"
    */
  val IN: Int = 31

  /**
    * Keyword "some"
    */
  val SOME: Int = 32

  /**
    * Keyword "every"
    */
  val EVERY: Int = 33

  /**
    * Keyword "satisfies"
    */
  val SATISFIES: Int = 34

  /**
    * Token representing the name of a function and the following "(" symbol
    */
  val FUNCTION: Int = 35

  /**
    * Token representing the name of an axis and the following "::" symbol
    */
  val AXIS: Int = 36

  /**
    * Keyword "if"
    */
  val IF: Int = 37

  /**
    * Operator "&lt;&lt;"
    */
  val PRECEDES: Int = 38

  /**
    * Operator "&gt;&gt;"
    */
  val FOLLOWS: Int = 39

  /**
    * Operator "!"
    */
  val BANG: Int = 40

  /**
    * "::" symbol
    */
  val COLONCOLON: Int = 41

  /**
    * ":*" symbol
    */
  val COLONSTAR: Int = 42

  /**
    * Token representing a function name and the following "#" symbol
    */
  val NAMED_FUNCTION_REF: Int = 43

  /**
    * # symbol
    */
  val HASH: Int = 44

  /**
    * operator "instance of"
    */
  val INSTANCE_OF: Int = 45

  /**
    * operator "cast as"
    */
  val CAST_AS: Int = 46

  /**
    * operator "treat as"
    */
  val TREAT_AS: Int = 47

// "Fortran" style comparison operators eq, ne, etc
  val FEQ: Int = 50

  /**
    * operator "ne"
    */
  val FNE: Int = 51

  /**
    * operator "gt"
    */
  val FGT: Int = 52

  /**
    * operator "lt"
    */
  val FLT: Int = 53

  /**
    * operator "ge"
    */
  val FGE: Int = 54

  /**
    * opeartor "le"
    */
  val FLE: Int = 55

  /**
    * operator "idiv"
    */
  val IDIV: Int = 56

  /**
    * operator "castable as"
    */
  val CASTABLE_AS: Int = 57

  /**
    * ":=" symbol (XQuery only)
    */
  val ASSIGN: Int = 58

  /**
    * "{" symbol (XQuery only)
    */
  val LCURLY: Int = 59

  /**
    * composite token: &lt;keyword "{"&gt; (XQuery only)
    */
  val KEYWORD_CURLY: Int = 60

  /**
    * composite token &lt;'element' QNAME&gt; (XQuery only)
    */
  val ELEMENT_QNAME: Int = 61

  /**
    * composite token &lt;'attribute' QNAME&gt; (XQuery only)
    */
  val ATTRIBUTE_QNAME: Int = 62

  /**
    * composite token &lt;'pi' QNAME&gt; (XQuery only)
    */
  val PI_QNAME: Int = 63

  /**
    * composite token &lt;'namespace' QNAME&gt; (XQuery only)
    */
  val NAMESPACE_QNAME: Int = 64

  /**
    * Keyword "typeswitch"
    */
  val TYPESWITCH: Int = 65

  /**
    * Keyword "switch" (XQuery 1.1)
    */
  val SWITCH: Int = 66

  /**
    * Keyword "case"
    */
  val CASE: Int = 67

  /**
    * Keyword "modify"
    */
  val MODIFY: Int = 68

  /**
    * Node kind, e.g. "node()" or "comment()"
    */
  val NODEKIND: Int = 69

// e.g. *:suffix - the suffix is actually a separate token
  val SUFFIX: Int = 70

  /**
    * "as" (in XQuery Update rename expression)
    */
  val AS: Int = 71

  /*
   * "group by" (XQuery 3.0)
   */

  val GROUP_BY: Int = 72

  /**
    * "for tumbling" (XQuery 3.0)
    */
  val FOR_TUMBLING: Int = 73

  /**
    * "for sliding" (XQuery 3.0)
    */
  val FOR_SLIDING: Int = 74

  /**
    * "for member" (Saxon extension)
    */
  val FOR_MEMBER: Int = 75

  /**
    * ":" (XPath 3.0 maps)
    */
  val COLON: Int = 76

  /**
    * Arrow operator "=&gt;" (XQuery 3.1)
    */
  val ARROW: Int = 77

  /**
    * First part of a string template. Token value includes all the text from ``[ up to the first `{
    */
  val STRING_CONSTRUCTOR_INITIAL: Int = 78

  /**
    * "otherwise" (Saxon extension)
    */
  val OTHERWISE: Int = 79

  /**
    * "andAlso" (Saxon extension)
    */
  val AND_ALSO: Int = 80

  /**
    * "orElse" (Saxon extension)
    */
  val OR_ELSE: Int = 81

  /**
    * "xquery version"
    */
  val XQUERY_VERSION: Int = 88

  /**
    * "xquery encoding"
    */
  val XQUERY_ENCODING: Int = 89

  /**
    * "declare namespace"
    */
  val DECLARE_NAMESPACE: Int = 90

  /**
    * "declare default"
    */
  val DECLARE_DEFAULT: Int = 91

  /**
    * "declare construction"
    */
  val DECLARE_CONSTRUCTION: Int = 92

  /**
    * "declare base-uri"
    */
  val DECLARE_BASEURI: Int = 93

  /**
    * "declare boundary-space"
    */
  val DECLARE_BOUNDARY_SPACE: Int = 94

  /**
    * "declare decimal-format"
    */
  val DECLARE_DECIMAL_FORMAT: Int = 95

  /**
    * "import schema"
    */
  val IMPORT_SCHEMA: Int = 96

  /**
    * "import module"
    */
  val IMPORT_MODULE: Int = 97

  /**
    * "declare variable"
    */
  val DECLARE_VARIABLE: Int = 98

  /**
    * "declare context"
    */
  val DECLARE_CONTEXT: Int = 99

  /**
    * "declare function"
    */
  val DECLARE_FUNCTION: Int = 100

  /**
    * "module namespace"
    */
  val MODULE_NAMESPACE: Int = 101

  /**
    * Various compound symbols supporting XQuery validation expression
    */
  val VALIDATE: Int = 102

  val VALIDATE_STRICT: Int = 103

  val VALIDATE_LAX: Int = 104

  val VALIDATE_TYPE: Int = 105

  /**
    * percent sign '%'
    */
  val PERCENT: Int = 106

  /**
    * "declare xmlspace"
    */
  val DECLARE_ORDERING: Int = 107

  /**
    * "declare copy-namespaces"
    */
  val DECLARE_COPY_NAMESPACES: Int = 108

  /**
    * "declare option"
    */
  val DECLARE_OPTION: Int = 109

  /**
    * "declare revalidation"
    */
  val DECLARE_REVALIDATION: Int = 110

  /**
    * "insert node/nodes"
    */
  val INSERT_NODE: Int = 111

  /**
    * "delete node/nodes"
    */
  val DELETE_NODE: Int = 112

  /**
    * "replace node/nodes"
    */
  val REPLACE_NODE: Int = 113

  /**
    * "replace value"
    */
  val REPLACE_VALUE: Int = 114

  /**
    * "rename node"
    */
  val RENAME_NODE: Int = 115

  /**
    * "first into"
    */
  val FIRST_INTO: Int = 116

  /**
    * "last into"
    */
  val LAST_INTO: Int = 117

  /**
    * "after"
    */
  val AFTER: Int = 118

  /**
    * "before"
    */
  val BEFORE: Int = 119

  /**
    * "into"
    */
  val INTO: Int = 120

  /**
    * "with"
    */
  val WITH: Int = 121

  /**
    * "declare updating [function]"
    */
  val DECLARE_UPDATING: Int = 122

  /**
    * declare %
    */
  val DECLARE_ANNOTATED: Int = 123

  /**
    * Saxon extension: declare type
    */
  val DECLARE_TYPE: Int = 124

  /**
    * semicolon separator
    */
  val SEMICOLON: Int = 149

  /**
    * Constant identifying the token number of the last token to be classified as an operator
    */
  var LAST_OPERATOR: Int = 150

  /**
    * Name token (a QName, in general)
    */
  val NAME: Int = 201

  /**
    * String literal
    */
  val STRING_LITERAL: Int = 202

  /**
    * Right square bracket
    */
  val RSQB: Int = 203

  /**
    * Right parenthesis
    */
  val RPAR: Int = 204

  /**
    * "." symbol
    */
  val DOT: Int = 205

  /**
    * ".." symbol
    */
  val DOTDOT: Int = 206

  /**
    * "*" symbol when used as a wildcard
    */
  val STAR: Int = 207

// e.g. prefix:*
  val PREFIX: Int = 208

  /**
    * Numeric literal
    */
  val NUMBER: Int = 209

  /**
    * "for" keyword
    */
  val FOR: Int = 211

  /**
    * Keyword "default"
    */
  val DEFAULT: Int = 212

  /**
    * Question mark symbol. That is, "?"
    */
  val QMARK: Int = 213

  /**
    * "}" symbol (XQuery only)
    */
  val RCURLY: Int = 215

  /**
    * "let" keyword (XQuery only)
    */
  val LET: Int = 216

  /**
    * "&lt;" at the start of a tag (XQuery only). The pseudo-XML syntax that
    * follows is read character-by-character by the XQuery parser
    */
  val TAG: Int = 217

  /**
    * A token representing an XQuery pragma.
    * This construct "(# .... #)" is regarded as a single token, for the QueryParser to sort out.
    */
  val PRAGMA: Int = 218

  /**
    * "copy" keyword
    */
  val COPY: Int = 219

  /**
    * "count" keyword
    */
  val COUNT: Int = 220

  /**
    * Complete string template with no embedded expressions
    */
  val STRING_LITERAL_BACKTICKED: Int = 222

// unary minus: not actually a token, but we
  val NEGATE: Int = 299

  val tokens: Array[String] = new Array[String](300)

  tokens(EOF)                        = "<eof>"
  tokens(UNION)                      = "|"
  tokens(SLASH)                      = "/"
  tokens(AT)                         = "@"
  tokens(LSQB)                       = "["
  tokens(LPAR)                       = "("
  tokens(EQUALS)                     = "="
  tokens(COMMA)                      = ","
  tokens(SLASH_SLASH)                = "//"
  tokens(OR)                         = "or"
  tokens(AND)                        = "and"
  tokens(GT)                         = ">"
  tokens(LT)                         = "<"
  tokens(GE)                         = ">="
  tokens(LE)                         = "<="
  tokens(PLUS)                       = "+"
  tokens(MINUS)                      = "-"
  tokens(MULT)                       = "*"
  tokens(DIV)                        = "div"
  tokens(MOD)                        = "mod"
  tokens(IS)                         = "is"
  tokens(DOLLAR)                     = "$"
  tokens(NE)                         = "!="
  tokens(BANG)                       = "!"
  tokens(CONCAT)                     = "||"
  tokens(INTERSECT)                  = "intersect"
  tokens(EXCEPT)                     = "except"
  tokens(RETURN)                     = "return"
  tokens(THEN)                       = "then"
  tokens(ELSE)                       = "else"
  tokens(TO)                         = "to"
  tokens(IN)                         = "in"
  tokens(SOME)                       = "some"
  tokens(EVERY)                      = "every"
  tokens(SATISFIES)                  = "satisfies"
  tokens(FUNCTION)                   = "<function>("
  tokens(AXIS)                       = "<axis>"
  tokens(IF)                         = "if("
  tokens(PRECEDES)                   = "<<"
  tokens(FOLLOWS)                    = ">>"
  tokens(COLONCOLON)                 = "::"
  tokens(COLONSTAR)                  = ":*"
  tokens(HASH)                       = "#"
  tokens(INSTANCE_OF)                = "instance of"
  tokens(CAST_AS)                    = "cast as"
  tokens(TREAT_AS)                   = "treat as"
  tokens(FEQ)                        = "eq"
  tokens(FNE)                        = "ne"
  tokens(FGT)                        = "gt"
  tokens(FGE)                        = "ge"
  tokens(FLT)                        = "lt"
  tokens(FLE)                        = "le"
  tokens(IDIV)                       = "idiv"
  tokens(CASTABLE_AS)                = "castable as"
  tokens(ASSIGN)                     = ":="
  tokens(SWITCH)                     = "switch"
  tokens(TYPESWITCH)                 = "typeswitch"
  tokens(CASE)                       = "case"
  tokens(DEFAULT)                    = "default"
//tokens [ AS_LAST ]                  = "as last";
  tokens(AFTER)                      = "after"
  tokens(BEFORE)                     = "before"
  tokens(INTO)                       = "into"
  tokens(WITH)                       = "with"
  tokens(MODIFY)                     = "modify"
  tokens(AS)                         = "as"
  tokens(COLON)                      = ":"
  tokens(ARROW)                      = "=>"
  tokens(AND_ALSO)                   = "andAlso"
  tokens(OR_ELSE)                    = "orElse"
  tokens(STRING_CONSTRUCTOR_INITIAL) = "``[<string>`{"
  tokens(STRING_LITERAL_BACKTICKED)  = "``[<string>]``"
  tokens(OTHERWISE)                  = "otherwise"
  tokens(NAME)                       = "<name>"
  tokens(STRING_LITERAL)             = "<string-literal>"
  tokens(RSQB)                       = "]"
  tokens(RPAR)                       = ")"
  tokens(DOT)                        = "."
  tokens(DOTDOT)                     = ".."
  tokens(STAR)                       = "*"
  tokens(PREFIX)                     = "<prefix:*>"
  tokens(NUMBER)                     = "<numeric-literal>"
  tokens(NODEKIND)                   = "<node-type>()"
  tokens(FOR)                        = "for"
  tokens(SUFFIX)                     = "<*:local-name>"
  tokens(QMARK)                      = "?"
  tokens(LCURLY)                     = "{"
  tokens(KEYWORD_CURLY)              = "<keyword> {"
  tokens(RCURLY)                     = "}"
  tokens(LET)                        = "let"
  tokens(VALIDATE)                   = "validate {"
  tokens(TAG)                        = "<element>"
  tokens(PRAGMA)                     = "(# ... #)"
  tokens(SEMICOLON)                  = ";"
  tokens(COPY)                       = "copy"
  tokens(NEGATE)                     = "-"
  tokens(PERCENT)                    = "%"

  /**
    * Lookup table for composite (two-keyword) tokens
    */ /**
    * Lookup table for composite (two-keyword) tokens
    */
  var doubleKeywords: HashMap[String, Integer] = new HashMap(30)

  /**
    * Pseudo-token representing the start of the expression
    */ /**
    * Pseudo-token representing the start of the expression
    */
  val UNKNOWN: Int = -1

  mapDouble("instance of",             INSTANCE_OF)
  mapDouble("cast as",                 CAST_AS)
  mapDouble("treat as",                TREAT_AS)
  mapDouble("castable as",             CASTABLE_AS)
  mapDouble("group by",                GROUP_BY)
  mapDouble("for tumbling",            FOR_TUMBLING)
  mapDouble("for sliding",             FOR_SLIDING)
  mapDouble("for member",              FOR_MEMBER)
  mapDouble("xquery version",          XQUERY_VERSION)
  mapDouble("xquery encoding",         XQUERY_ENCODING)
  mapDouble("declare namespace",       DECLARE_NAMESPACE)
  mapDouble("declare default",         DECLARE_DEFAULT)
  mapDouble("declare construction",    DECLARE_CONSTRUCTION)
  mapDouble("declare base-uri",        DECLARE_BASEURI)
  mapDouble("declare boundary-space",  DECLARE_BOUNDARY_SPACE)
  mapDouble("declare decimal-format",  DECLARE_DECIMAL_FORMAT)
  mapDouble("declare ordering",        DECLARE_ORDERING)
  mapDouble("declare copy-namespaces", DECLARE_COPY_NAMESPACES)
  mapDouble("declare option",          DECLARE_OPTION)
  mapDouble("declare revalidation",    DECLARE_REVALIDATION)
  // Saxon extension
  mapDouble("declare type",            DECLARE_TYPE)
  mapDouble("import schema",           IMPORT_SCHEMA)
  mapDouble("import module",           IMPORT_MODULE)
  mapDouble("declare variable",        DECLARE_VARIABLE)
  mapDouble("declare context",         DECLARE_CONTEXT)
  mapDouble("declare function",        DECLARE_FUNCTION)
  mapDouble("declare updating",        DECLARE_UPDATING)
  mapDouble("module namespace",        MODULE_NAMESPACE)
  mapDouble("validate strict",         VALIDATE_STRICT)
  mapDouble("validate lax",            VALIDATE_LAX)
  mapDouble("validate type",           VALIDATE_TYPE)
  mapDouble("insert node",             INSERT_NODE)
  mapDouble("insert nodes",            INSERT_NODE)
  mapDouble("delete node",             DELETE_NODE)
  mapDouble("delete nodes",            DELETE_NODE)
  mapDouble("replace node",            REPLACE_NODE)
  mapDouble("replace value",           REPLACE_VALUE)
  mapDouble("rename node",             RENAME_NODE)
  mapDouble("rename nodes",            RENAME_NODE)
  mapDouble("first into",              FIRST_INTO)
  mapDouble("last into",               LAST_INTO)

  private def mapDouble(doubleKeyword: String, token: Int): Unit = {
    doubleKeywords.put(doubleKeyword, token)
    tokens(token) = doubleKeyword
  }

  def inverse(operator: Int): Int = operator match {
    case LT  => GT
    case LE  => GE
    case GT  => LT
    case GE  => LE
    case FLT => FGT
    case FLE => FGE
    case FGT => FLT
    case FGE => FLE
    case _   => operator
  }

  def negate(operator: Int): Int = operator match {
    case FEQ => FNE
    case FNE => FEQ
    case FLT => FGE
    case FLE => FGT
    case FGT => FLE
    case FGE => FLT
    case _   => throw new IllegalArgumentException("Invalid operator for negate()")
  }

  def isOrderedOperator(operator: Int): Boolean =
    operator != FEQ && operator != FNE
}
