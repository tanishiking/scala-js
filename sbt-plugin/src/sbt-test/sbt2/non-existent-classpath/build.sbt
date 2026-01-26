version := scalaJSVersion
scalaVersion := "3.3.4"

enablePlugins(ScalaJSPlugin)

// Test that non-existent classpath entries are allowed - #2198
// In sbt 2.x, fullClasspath uses HashedVirtualFileRef, so we need FileConverter
// Also needs Def.uncached to avoid caching issues with non-existent files
Compile / fullClasspath ++= Def.uncached {
  val conv = fileConverter.value
  val f = baseDirectory.value / "non-existent-directory-please-dont-ever-create-this"
  Seq(Attributed.blank(conv.toVirtualFile(f.toPath)))
}

scalaJSUseMainModuleInitializer := true
