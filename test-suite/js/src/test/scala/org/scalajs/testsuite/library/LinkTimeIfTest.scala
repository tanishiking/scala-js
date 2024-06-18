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

import scala.scalajs.LinkingInfo
import scala.scalajs.LinkingInfo.ESVersion

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.Platform

class LinkTimeIfTest {
  @Test def linkTimeIfBasic(): Unit = {
    // boolean const
    LinkingInfo.linkTimeIf(true) { /* ok */ } { fail() }
    LinkingInfo.linkTimeIf(false) { fail() } { /* ok */ }

    // boolean property
    LinkingInfo.linkTimeIf(LinkingInfo.productionMode) {
      if (Platform.isInProductionMode) { /* ok */ }
      else fail()
    } {
      if (!Platform.isInProductionMode) { /* ok */ }
      else fail()
    }
    LinkingInfo.linkTimeIf(!LinkingInfo.productionMode) {
      if (!Platform.isInProductionMode) { /* ok */ }
      else fail()
    } {
      if (Platform.isInProductionMode) { /* ok */ }
      else fail()
    }

    // integer property
    // should be always true because ES5_1 is the oldest version we support
    LinkingInfo.linkTimeIf(LinkingInfo.esVersion >= ESVersion.ES5_1) {
      /* ok */
    } {
      fail()
    }
  }
}
