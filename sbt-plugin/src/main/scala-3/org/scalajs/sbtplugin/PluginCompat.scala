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

import sbt.*
import sbt.Keys.*
import sbt.librarymanagement.DependencyResolution
import lmcoursier.CoursierDependencyResolution
import sbt.internal.util.StringAttributeKey
import xsbti.{FileConverter, HashedVirtualFileRef, VirtualFile, VirtualFileRef}

import org.scalajs.linker.interface.Report

import ScalaJSPlugin.autoImport.*

/** Compatibility shim for sbt 2.x (Scala 3). */
private[sbtplugin] object PluginCompat {
  type FileRef = HashedVirtualFileRef
  type Out = VirtualFile
  type ArtifactPath = VirtualFileRef
  type FC = FileConverter

  // Get FileConverter from sbt - available as setting in sbt 2.x
  def fileConverter: Def.Initialize[FileConverter] =
    Def.setting(Keys.fileConverter.value)

  // def toNioPath(f: HashedVirtualFileRef)(using conv: FileConverter): NioPath =
  //   conv.toPath(f)
  def virtualFileRefToFile(ref: VirtualFileRef)(using conv: FileConverter): File =
    conv.toPath(ref).toFile

  def fileToVirtualFileRef(f: File)(using conv: FileConverter): VirtualFileRef =
    conv.toVirtualFile(f.toPath)

  def toFiles(cp: Seq[Attributed[HashedVirtualFileRef]])(using conv: FileConverter): Seq[File] =
    cp.map(a => conv.toPath(a.data).toFile)

  def toFile(a: Attributed[HashedVirtualFileRef])(using conv: FileConverter): File =
    conv.toPath(a.data).toFile

  def toAttributedFiles(files: Seq[File])(
      using conv: FileConverter): Seq[Attributed[HashedVirtualFileRef]] = {
    Attributed.blankSeq(files.map(f => conv.toVirtualFile(f.toPath)))
  }

  def sjsirFile(classFile: File): File =
    new File(classFile.getPath.stripSuffix(".class") + ".sjsir")

  // Dependency resolution helper for custom fullClasspath
  // In sbt 2.x, use Coursier directly via csrResolvers
  def dependencyResolutionValue(
      unused: Def.Initialize[Task[DependencyResolution]]
  ): Def.Initialize[Task[DependencyResolution]] = {
    // In sbt 2.x, get csrConfiguration from the root project
    Def.task {
      val csrConfig = (LocalRootProject / csrConfiguration).value
      CoursierDependencyResolution(csrConfig)
    }
  }

  // Platform deps cross version setting - not needed in sbt 2.x
  // In sbt 2.x, %% is platform-aware by default
  def platformDepsCrossVersionSetting: Seq[Setting[?]] = Seq.empty

  // Attributed operations - sbt 2.x uses StringAttributeKey
  // Store File as path string
  def attributedPutFile[T](a: Attributed[T], key: AttributeKey[File], value: File): Attributed[T] =
    a.put(StringAttributeKey(key.label), value.getAbsolutePath)

  def attributedGetFile[T](a: Attributed[T], key: AttributeKey[File]): Option[File] =
    a.get(StringAttributeKey(key.label)).map(path => new File(path))

  def attributedPutFiles[T](a: Attributed[T], key: AttributeKey[Seq[File]],
      value: Seq[File]): Attributed[T] = {
    a.put(StringAttributeKey(key.label), value.map(_.getAbsolutePath).mkString("\u0000"))
  }

  def attributedGetFiles[T](a: Attributed[T], key: AttributeKey[Seq[File]]): Option[Seq[File]] = {
    a.get(StringAttributeKey(key.label)).map(
        s => if (s.isEmpty) Nil else s.split("\u0000").toSeq.map(new File(_)))
  }

  def attributedPutValue[T, V](a: Attributed[T], key: AttributeKey[V], value: V): Attributed[T] =
    a.put(StringAttributeKey(key.label), value.toString)

  def attributedGetValue[T](a: Attributed[T], key: StringAttributeKey): Option[String] =
    a.get(key)

  // Linker artifact suffix - use 2.13 for sbt 2.x (Scala 3 can load 2.13 JARs)
  val linkerScalaSuffix: String = "_2.13"

  // Dummy implicit class to satisfy import - Def.uncached is built-in in sbt 2.x
  implicit class DefOps(singleton: Def.type)

  /** Detect if linker config changed since previous run.
   *
   *  sbt 2.x implementation: Uses file-based caching to store fingerprints on disk.
   *  The .previous macro from sbt 1.x doesn't exist in sbt 2.x, so we manually
   *  persist fingerprints to files and compare them on subsequent runs.
   *
   *  Note: In the future, sbt 2.x's built-in remote/disk caching mechanism may
   *  handle this automatically, at which point this workaround could be removed.
   *  For now, we need this to ensure config changes trigger re-linking.
   *
   *  @param cacheDirectory directory to store fingerprint files
   *  @param moduleInitFingerprints current fingerprints of module initializers
   *  @param prevModuleInitFingerprints unused in sbt 2.x (always None)
   *  @param linkerConfigFingerprint current fingerprint of linker config
   *  @param prevLinkerConfigFingerprint unused in sbt 2.x (always None)
   *  @return true if config changed and re-linking is needed
   */
  def detectConfigChange(
      cacheDirectory: File,
      moduleInitFingerprints: Seq[String],
      prevModuleInitFingerprints: Option[Seq[String]],
      linkerConfigFingerprint: String,
      prevLinkerConfigFingerprint: Option[String]
  ): Boolean = {
    val moduleInitFingerprintFile = cacheDirectory / "module-init-fingerprints.txt"
    val linkerConfigFingerprintFile = cacheDirectory / "linker-config-fingerprint.txt"

    def fingerprintChanged(file: File, current: String): Boolean = {
      if (file.exists()) {
        val previous = IO.read(file)
        previous != current
      } else {
        false
      }
    }

    val moduleInitStr = moduleInitFingerprints.mkString(",")
    val moduleInitializersChanged = fingerprintChanged(moduleInitFingerprintFile, moduleInitStr)
    val linkerConfigChanged =
      fingerprintChanged(linkerConfigFingerprintFile, linkerConfigFingerprint)

    // Update stored fingerprints for next run
    IO.write(moduleInitFingerprintFile, moduleInitStr)
    IO.write(linkerConfigFingerprintFile, linkerConfigFingerprint)

    moduleInitializersChanged || linkerConfigChanged
  }

  /** Extension to provide .previous on TaskKey in sbt 2.x.
   *  In sbt 2.x, .previous doesn't exist, so we return None.
   *  File-based caching in detectConfigChange handles the actual change detection.
   */
  implicit class TaskKeyPreviousOps[T](private val key: TaskKey[T]) extends AnyVal {
    def previous: Option[T] = None
  }
}
