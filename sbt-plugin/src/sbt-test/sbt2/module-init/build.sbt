import org.scalajs.linker.interface.ModuleInitializer

val check = taskKey[Unit]("Run checks of this test")

version := scalaJSVersion
scalaVersion := "3.3.4"

enablePlugins(ScalaJSPlugin)

scalaJSUseMainModuleInitializer := true
scalaJSModuleInitializers += Def.uncached {
  ModuleInitializer.mainMethod("org.scalajs.sbtplugin.test", "foo")
}
Test / scalaJSModuleInitializers += Def.uncached {
  ModuleInitializer.mainMethod("org.scalajs.sbtplugin.test", "bar")
}

check := Def.uncached {
  // Compile should have main module init and Main.foo
  assert((Compile / scalaJSModuleInitializers).value.size == 2,
      "Bad number of Compile / scalaJSModuleInitializers")

  // Test should have test module init, Main.foo and Main.bar
  assert((Test / scalaJSModuleInitializers).value.size == 3,
      "Bad number of Test / scalaJSModuleInitializers")
}
