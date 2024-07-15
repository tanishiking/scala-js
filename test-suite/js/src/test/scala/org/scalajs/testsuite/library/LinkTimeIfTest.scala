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

package org.scalajs.testsuite.library

import scala.scalajs.LinkingInfo._

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.Platform

class LinkTimeIfTest {
  @Test def linkTimeIfConst(): Unit = {
    // boolean const
    assertEquals(1, linkTimeIf(true) { 1 } { 2 })
    assertEquals(2, linkTimeIf(false) { 1 } { 2 })
  }

  @Test def linkTimeIfProp(): Unit = {
    locally {
      val cond = Platform.isInProductionMode
      assertEquals(cond, linkTimeIf(productionMode) { true } { false })
    }

    locally {
      val cond = !Platform.isInProductionMode
      assertEquals(cond, linkTimeIf(!productionMode) { true } { false })
    }
  }

  @Test def linkTimIfIntProp(): Unit = {
    locally {
      val cond = Platform.assumedESVersion >= ESVersion.ES2015
      assertEquals(cond, linkTimeIf(esVersion >= ESVersion.ES2015) { true } { false })
    }

    locally {
      val cond = !(Platform.assumedESVersion < ESVersion.ES2015)
      assertEquals(cond, linkTimeIf(!(esVersion < ESVersion.ES2015)) { true } { false })
    }
  }

  @Test def linkTimeIfNested(): Unit = {
    locally {
      val cond =
        Platform.isInProductionMode &&
        Platform.assumedESVersion >= ESVersion.ES2015
      assertEquals(cond,
          linkTimeIf(productionMode && esVersion >= ESVersion.ES2015) { true } { false })
    }

    locally {
      val cond =
        Platform.assumedESVersion >= ESVersion.ES2015 &&
        Platform.assumedESVersion < ESVersion.ES2019 &&
        Platform.isInProductionMode
      assertEquals(cond,
          linkTimeIf(
            esVersion >= ESVersion.ES2015 &&
            esVersion < ESVersion.ES2019 &&
            productionMode
          ) { true } { false })
    }
  }
}
