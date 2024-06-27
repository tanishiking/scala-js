/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.nscplugin.test

import util._

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.{Trees => js, Types => jstpe}
import org.scalajs.ir.Trees.LinkTimeTree._
import org.scalajs.ir.Trees.LinkTimeOp._

class LinkTimeIfTest extends JSASTTest with TestHelpers {
  override def preamble: String = "import scala.scalajs.LinkingInfo._"

  @Test
  def linkTimeIfConst(): Unit = {
    def test(const: String, expected: Boolean) = {
      s"""
      $preamble
      object A {
        def foo = {
          linkTimeIf($const) { } { }
        }
      }
      """.hasExactly(1, "LinkTimeIf with boolean constant") {
        case js.LinkTimeIf(BooleanConst(expected), _, _) =>
      }
    }
    test("true", true)
    test("false", false)
  }

  @Test
  def linkTimeIfSimple(): Unit = {
    s"""
    $preamble
    object A {
      def foo = {
        linkTimeIf(productionMode) { "prod" } { "dev" }
      }
    }
    """.hasExactly(1, "LinkTimeIf with a property refers core/productionMode") {
      case js.LinkTimeIf(
        Property("core/productionMode", jstpe.BooleanType), _, _) =>
    }
  }

  @Test
  def linkTimeIfNot: Unit = {
    s"""
    $preamble
    object A {
      def foo = {
        linkTimeIf(!productionMode) { "prod" } { "dev" }
      }
    }
    """.hasExactly(1, "LinkTimeIf whose condition checks productionMode is false") {
      case js.LinkTimeIf(
        BinaryOp(
          Boolean_==,
          Property("core/productionMode", jstpe.BooleanType),
          BooleanConst(false)
        ), _, _
      ) =>
    }
  }

  @Test
  def linkTimeIfCompareInt: Unit = {
    def test(opStr: String, op: js.LinkTimeOp.Code): Unit = {
      s"""
      $preamble
      object A {
        def foo = {
          linkTimeIf(esVersion $opStr ESVersion.ES2015 ) { } { }
        }
      }
      """.hasExactly(1, s"LinkTimeIf compares esVersion using '$opStr'") {
        case js.LinkTimeIf(
          BinaryOp(
            op,
            Property("core/esVersion", jstpe.IntType),
            IntConst(_)
          ), _, _
        ) =>
      }
    }
    test("==", Int_==)
    test("!=", Int_!=)
    test("<",  Int_<)
    test("<=", Int_<=)
    test(">",  Int_>)
    test(">=", Int_>=)
  }

  @Test
  def linkTimeIfNestedSimple(): Unit = {
    def test(op: String, expected: js.LinkTimeOp.Code): Unit = {
      s"""
      $preamble
      object A {
        def foo = {
          linkTimeIf(productionMode && esVersion >= ESVersion.ES2015 ) { } { }
        }
      }
      """.hasExactly(1, s"LinkTimeIf with nested condition") {
        case js.LinkTimeIf(
          js.LinkTimeTree.BinaryOp(
            expected,
            Property("core/productionMode", jstpe.BooleanType),
            BinaryOp(
              Int_>=,
              Property("core/esVersion", jstpe.IntType),
              _
            )
          ), _, _
        ) =>
      }
    }
    test("&&", Boolean_&&)
    test("||", Boolean_||)
  }

  @Test
  def linkTimeIfNested(): Unit = {
    s"""
    $preamble
    object A {
      def foo = {
        linkTimeIf(productionMode == (productionMode && productionMode)) { } { }
      }
    }
    """.hasExactly(1, s"LinkTimeIf with nested condition") {
      case js.LinkTimeIf(
        js.LinkTimeTree.BinaryOp(
          Boolean_==,
          Property("core/productionMode", jstpe.BooleanType),
          BinaryOp(
            Boolean_&&,
            Property("core/productionMode", jstpe.BooleanType),
            Property("core/productionMode", jstpe.BooleanType)
          )
        ), _, _
      ) =>
    }

    s"""
    $preamble
    object A {
      def foo = {
        linkTimeIf(
          (esVersion >= ESVersion.ES2015 &&
           esVersion <= ESVersion.ES2019) ||
           productionMode
        ) { } { }
      }
    }
    """.hasExactly(1, s"LinkTimeIf with nested condition") {
      case js.LinkTimeIf(
        js.LinkTimeTree.BinaryOp(
          Boolean_||,
          BinaryOp(
            Boolean_&&,
            BinaryOp(Int_>=, Property("core/esVersion", jstpe.IntType), _),
            BinaryOp(Int_<=, Property("core/esVersion", jstpe.IntType), _)
          ),
          Property("core/productionMode", jstpe.BooleanType)
        ), _, _
      ) =>
    }
  }

  // scalastyle:off line.size.limit
  @Test
  def linkTimeErrorInvalidOp(): Unit = {
    """
    object A {
      def foo =
        linkTimeIf((esVersion + 1) < ESVersion.ES2015) { } { }
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Invalid operation '$plus' inside linkTimeIf. Only '==', '!=', '>', '>=', '<', '<=' operations are allowed for integer values in linkTimeIf.
      |        linkTimeIf((esVersion + 1) < ESVersion.ES2015) { } { }
      |                              ^
    """

    """
    object A {
      def foo =
        linkTimeIf(productionMode | true) { } { }
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Invalid operation '$bar' inside linkTimeIf. Only '==' and '!=' operations are allowed for boolean values in linkTimeIf.
      |        linkTimeIf(productionMode | true) { } { }
      |                                  ^
    """
  }

  @Test
  def linkTimeErrorInvalidEntities(): Unit = {
    """
    object A {
      def foo(x: String) = {
        val bar = 1
        linkTimeIf(bar == 0) { } { }
      }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: Invalid identifier bar inside linkTimeIf. Only @linkTimeProperty annotated values can be used in linkTimeIf.
      |        linkTimeIf(bar == 0) { } { }
      |                   ^
    """

    """
    object A {
      def foo(x: String) =
        linkTimeIf("foo" == x) { } { }
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Invalid literal "foo" inside linkTimeIf. Only boolean and int values can be used in linkTimeIf.
      |        linkTimeIf("foo" == x) { } { }
      |                   ^
      |newSource1.scala:4: error: Invalid identifier x inside linkTimeIf. Only @linkTimeProperty annotated values can be used in linkTimeIf.
      |        linkTimeIf("foo" == x) { } { }
      |                            ^
    """

    """
    object A {
      def bar = true
      def foo(x: String) =
        linkTimeIf(bar || !bar) { } { }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: Invalid identifier inside linkTimeIf. Only @linkTimeProperty annotated values can be used in linkTimeIf.
      |        linkTimeIf(bar || !bar) { } { }
      |                   ^
      |newSource1.scala:5: error: Invalid identifier inside linkTimeIf. Only @linkTimeProperty annotated values can be used in linkTimeIf.
      |        linkTimeIf(bar || !bar) { } { }
      |                           ^
    """
  }

  @Test
  def linkTimeCondInvalidTree(): Unit = {
    """
    object A {
      def bar = true
      def foo(x: String) =
        linkTimeIf(if(bar) true else false) { } { }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: Only @linkTimeProperty annotated values, int and boolean constants, and binary operations are allowd in linkTimeIf.
      |        linkTimeIf(if(bar) true else false) { } { }
      |                   ^
    """
  }
}
