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

package org.scalajs.linker.standard

import org.scalajs.ir.{Types => jstpe, Trees => js}
import org.scalajs.ir.ScalaJSVersions
import org.scalajs.ir.Position.NoPosition
import org.scalajs.linker.interface.{Semantics, ESFeatures}

final class LinkTimeProperties private[standard] (
  semantics: Semantics,
  esFeatures: ESFeatures,
  targetIsWebAssembly: Boolean
) {
  import LinkTimeProperties._
  import LinkTimeValue._

  private val linkTimeProperties: Map[String, LinkTimeValue] = Map(
    "core/esVersion" ->
      LinkTimeValue.LinkTimeInt(esFeatures.esVersion.edition),
    "core/assumingES6" ->
      LinkTimeValue.LinkTimeBoolean(esFeatures.useECMAScript2015Semantics),
    "core/isWebAssembly" ->
      LinkTimeValue.LinkTimeBoolean(targetIsWebAssembly),
    "core/productionMode" ->
      LinkTimeValue.LinkTimeBoolean(semantics.productionMode),
    "core/linkerVersion" ->
      LinkTimeValue.LinkTimeString(ScalaJSVersions.current)
  )

  def transformLinkTimeProperty(prop: js.LinkTimeProperty): js.Literal = {
    val value = linkTimeProperties.getOrElse(prop.name,
        throw new IllegalArgumentException(s"link time property not found: '$prop'"))
    value match {
      case LinkTimeBoolean(value) =>
        js.BooleanLiteral(value)(prop.pos)
      case LinkTimeInt(value) =>
        js.IntLiteral(value)(prop.pos)
      case LinkTimeString(value) =>
        js.StringLiteral(value)(prop.pos)
    }
  }
}

object LinkTimeProperties {
  sealed trait LinkTimeValue
  object LinkTimeValue {
    case class LinkTimeInt(value: Int) extends LinkTimeValue
    case class LinkTimeBoolean(value: Boolean) extends LinkTimeValue
    case class LinkTimeString(value: String) extends LinkTimeValue
  }
}
