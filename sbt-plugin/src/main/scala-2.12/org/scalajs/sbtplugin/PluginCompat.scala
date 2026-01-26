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

package org.scalajs.sbtplugin

import java.io.File

import sbt._
import sbt.Keys._
import sbt.librarymanagement.DependencyResolution

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

import org.scalajs.linker.interface.Report

import sjsonnew.BasicJsonProtocol._

import ScalaJSPlugin.autoImport._

/** Dummy FileConverter for sbt 1.x (not needed, File access is direct). */
sealed abstract class DummyFileConverter

private[sbtplugin] object DummyFileConverter extends DummyFileConverter {
  implicit val dummyFileConverter: DummyFileConverter = DummyFileConverter
}

/** Compatibility shim for sbt 1.x (Scala 2.12). */
private[sbtplugin] object PluginCompat {
  type FileRef = File
  type Out = File
  type ArtifactPath = File
  type FC = DummyFileConverter

  // Re-export implicit for convenience
  implicit def dummyFileConverter: FC = DummyFileConverter.dummyFileConverter

  // Get FileConverter - returns dummy in sbt 1.x
  def fileConverter: Def.Initialize[FC] =
    Def.setting(DummyFileConverter.dummyFileConverter)

  def virtualFileRefToFile(f: File)(implicit dummy: FC): File = f
  def fileToVirtualFileRef(f: File)(implicit dummy: FC): File = f

  def toFiles(cp: Seq[Attributed[File]])(implicit dummy: FC): Seq[File] =
    Attributed.data(cp)

  def toFile(a: Attributed[File])(implicit dummy: FC): File =
    a.data

  def toAttributedFiles(files: Seq[File])(implicit dummy: FC): Seq[Attributed[File]] =
    Attributed.blankSeq(files)

  def sjsirFile(classFile: File): File =
    new File(classFile.getPath.stripSuffix(".class") + ".sjsir")

  // Dependency resolution helper for custom fullClasspath
  def dependencyResolutionValue(
      dependencyResolution: Def.Initialize[Task[DependencyResolution]]
  ): Def.Initialize[Task[DependencyResolution]] = {
    Def.task {
      val log = streams.value.log
      import sbt.librarymanagement.ivy._
      val ivyConfig = InlineIvyConfiguration()
        .withResolvers(Vector(Resolver.defaultLocal, Resolver.mavenCentral))
        .withLog(log)
      IvyDependencyResolution(ivyConfig)
    }
  }

  // Platform deps cross version setting (sbt-platform-deps plugin)
  def platformDepsCrossVersionSetting: Seq[Setting[_]] = Seq(
    platformDepsCrossVersion := ScalaJSCrossVersion.binary
  )

  def attributedPutFile[T](a: Attributed[T], key: AttributeKey[File], value: File): Attributed[T] =
    a.put(key, value)

  def attributedGetFile[T](a: Attributed[T], key: AttributeKey[File]): Option[File] =
    a.get(key)

  def attributedPutFiles[T](a: Attributed[T], key: AttributeKey[Seq[File]],
      value: Seq[File]): Attributed[T] = {
    a.put(key, value)
  }

  def attributedGetFiles[T](a: Attributed[T], key: AttributeKey[Seq[File]]): Option[Seq[File]] =
    a.get(key)

  def attributedPutValue[T, V](a: Attributed[T], key: AttributeKey[V], value: V): Attributed[T] =
    a.put(key, value)

  // This adds `Def.uncached(...)`
  implicit class DefOps(singleton: Def.type) {
    def uncached[A1](a: A1): A1 = a
  }

  // Linker artifact suffix - use 2.12 for sbt 1.x
  val linkerScalaSuffix: String = "_2.12"

  /** Detect if linker config changed since previous run.
   *  sbt 1.x implementation using .previous.
   *  Called from within a Def.taskDyn context with previous values passed in.
   */
  def detectConfigChange(
      cacheDirectory: File, // unused in sbt1, kept for API compatibility
      moduleInitFingerprints: Seq[String],
      prevModuleInitFingerprints: Option[Seq[String]],
      linkerConfigFingerprint: String,
      prevLinkerConfigFingerprint: Option[String]
  ): Boolean = {
    val moduleInitChanged = prevModuleInitFingerprints.exists(_ != moduleInitFingerprints)
    val linkerConfigChanged = prevLinkerConfigFingerprint.exists(_ != linkerConfigFingerprint)
    moduleInitChanged || linkerConfigChanged
  }

  // In sbt 1.x, .previous is available on TaskKey via sbt's built-in implicit.
  // This dummy class exists so that `import PluginCompat.TaskKeyPreviousOps` compiles,
  // but sbt's built-in implicit will take precedence when .previous is called.
  implicit class TaskKeyPreviousOps[T](private val key: TaskKey[T]) extends AnyVal {
    // This method won't be called - sbt's built-in .previous takes precedence
  }
}
