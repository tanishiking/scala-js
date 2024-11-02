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

package org.scalajs.linker.checker

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.ClassKind
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.logging.NullLogger

import org.scalajs.linker.interface.{LinkingException, StandardConfig}
import org.scalajs.linker.standard.{StandardLinkerFrontend, SymbolRequirement}

import org.scalajs.linker.testutils._
import org.scalajs.linker.testutils.TestIRBuilder._

import org.scalajs.junit.async.{AsyncResult, await}

class ClassDefCheckerTest {
  import ClassDefCheckerTest.assertError

  @Test
  def linkerActuallyFailsOnClassDefCheckerError(): AsyncResult = await {
    import scala.concurrent.ExecutionContext.Implicits.global

    val wrongClassDef = classDef(
      "A",
      kind = ClassKind.Interface,
      jsNativeLoadSpec = Some(JSNativeLoadSpec.Global("Foo", Nil))
    )

    val config = StandardConfig()
      .withCheckIR(true)
      .withOptimizer(false)
    val linkerFrontend = StandardLinkerFrontend(config)

    val loadASymbolRequirements = SymbolRequirement
      .factory("ClassDefCheckerTest")
      .classData("A")

    TestIRRepo.minilib.flatMap { stdLibFiles =>
      val irFiles = stdLibFiles :+ MemClassDefIRFile(wrongClassDef)
      linkerFrontend.link(irFiles, Nil, loadASymbolRequirements, NullLogger)
    }.failed.map { th =>
      assertTrue(th.toString(), th.isInstanceOf[LinkingException])
    }
  }

  @Test
  def javaLangObjectNoSuperClass(): Unit = {
    assertError(
        classDef(ObjectClass, superClass = Some("Parent")),
        "java.lang.Object cannot have a superClass")
  }

  @Test
  def javaLangObjectKind(): Unit = {
    assertError(
        classDef(ObjectClass, kind = ClassKind.Interface),
        "java.lang.Object must be a Class")
  }

  @Test
  def javaLangObjectNoInterfaces(): Unit = {
    assertError(
        classDef(ObjectClass, interfaces = List("Parent")),
        "java.lang.Object may not implement any interfaces")
  }

  @Test
  def hijackedClassesKinds(): Unit = {
    assertError(
        classDef(BoxedIntegerClass, kind = ClassKind.Class, superClass = Some(ObjectClass)),
        "java.lang.Integer must be a HijackedClass")

    assertError(
        classDef("A", kind = ClassKind.HijackedClass, superClass = Some(ObjectClass)),
        "A must not be a HijackedClass")
  }

  @Test
  def missingSuperClass(): Unit = {
    val kinds = Seq(
        ClassKind.Class,
        ClassKind.ModuleClass,
        ClassKind.HijackedClass,
        ClassKind.JSClass,
        ClassKind.JSModuleClass,
        ClassKind.NativeJSClass,
        ClassKind.NativeJSModuleClass
    )

    for (kind <- kinds) {
      val name = if (kind == ClassKind.HijackedClass) BoxedIntegerClass else ClassName("A")
      assertError(
          classDef(name, kind = kind,
              methods = requiredMethods(name, kind, parentClassName = name),
              jsConstructor = requiredJSConstructor(kind)),
          "missing superClass")
    }
  }

  @Test
  def interfaceNoSuperClass(): Unit = {
    assertError(
        classDef("A", kind = ClassKind.Interface, superClass = Some("B")),
        "interfaces may not have a superClass")
  }

  @Test
  def fieldDefClassName(): Unit = {
    assertError(
      classDef(
        "A",
        superClass = Some(ObjectClass),
        fields = List(
          FieldDef(EMF, FieldName("B", "foo"), NON, IntType)
        ),
        methods = List(trivialCtor("A"))
      ),
      "illegal FieldDef with name B::foo in class A"
    )

    // evidence that we do not need an explicit check for top-level field exports
    assertError(
      classDef(
        "A",
        kind = ClassKind.ModuleClass,
        superClass = Some(ObjectClass),
        fields = List(
          FieldDef(EMF.withNamespace(MemberNamespace.PublicStatic), FieldName("A", "foo"), NON, IntType)
        ),
        methods = List(trivialCtor("A")),
        topLevelExportDefs = List(
          TopLevelFieldExportDef("main", "foo", FieldName("B", "foo"))
        )
      ),
      "Cannot export non-existent static field 'B::foo'"
    )
  }

  @Test
  def noDuplicateFields(): Unit = {
    assertError(
        classDef("A", superClass = Some(ObjectClass),
            fields = List(
              FieldDef(EMF, FieldName("A", "foobar"), NON, IntType),
              FieldDef(EMF, FieldName("A", "foobar"), NON, BooleanType)
            )),
        "duplicate field 'A::foobar'")
  }

  @Test
  def illegalFieldTypes(): Unit = {
    val badFieldTypes: List[Type] = List(
      AnyNotNullType,
      ClassType(BoxedStringClass, nullable = false),
      ArrayType(ArrayTypeRef(I, 1), nullable = false),
      RecordType(List(RecordType.Field("I", NON, IntType, mutable = true))),
      NothingType,
      NoType
    )

    for (fieldType <- badFieldTypes) {
      assertError(
          classDef("A", superClass = Some(ObjectClass),
              fields = List(
                FieldDef(EMF, FieldName("A", "x"), NON, fieldType)
              )),
          s"FieldDef cannot have type ${fieldType.show()}")
    }
  }

  @Test
  def noDuplicateMethods(): Unit = {
    val babarMethodName = MethodName("babar", List(IntRef), IntRef)

    assertError(
        classDef("A", superClass = Some(ObjectClass),
          methods = List(
              MethodDef(EMF, babarMethodName, NON, List(paramDef("x", IntType)),
                      IntType, None)(EOH, UNV),
              MethodDef(EMF, babarMethodName, NON, List(paramDef("y", IntType)),
                      IntType, None)(EOH, UNV)
            )),
        "duplicate method 'babar(int)int'")
  }

  @Test
  def noDuplicateConstructors(): Unit = {
    val BoxedStringType = ClassType(BoxedStringClass, nullable = true)
    val stringCtorName = MethodName.constructor(List(T))

    val FooClass = ClassName("Foo")

    val callPrimaryCtorBody: Tree = {
      ApplyStatically(EAF.withConstructor(true), thisFor(FooClass),
          FooClass, NoArgConstructorName, Nil)(NoType)
    }

    assertError(
        classDef(FooClass, superClass = Some(ObjectClass),
            methods = List(
              trivialCtor(FooClass),
              MethodDef(EMF.withNamespace(MemberNamespace.Constructor),
                  stringCtorName, NON, List(paramDef("x", BoxedStringType)),
                  NoType, Some(callPrimaryCtorBody))(
                  EOH, UNV),
              MethodDef(EMF.withNamespace(MemberNamespace.Constructor),
                  stringCtorName, NON, List(paramDef("y", BoxedStringType)),
                  NoType, Some(callPrimaryCtorBody))(
                  EOH, UNV)
          )),
        "duplicate constructor method '<init>(java.lang.String)void'")
  }

  @Test
  def noStaticAbstractMethods(): Unit = {
    val fooMethodName = MethodName("foo", Nil, IntRef)

    assertError(
        classDef("A",
            kind = ClassKind.Interface,
            methods = List(
              MethodDef(EMF.withNamespace(MemberNamespace.PublicStatic),
                  fooMethodName, NON, Nil, IntType, None)(EOH, UNV),
              MethodDef(EMF, fooMethodName, NON, Nil, IntType, None)(EOH, UNV) // OK
            )
        ),
        "Abstract methods may only be in the public namespace")
  }

  @Test
  def publicReflectiveProxy(): Unit = {
    val babarMethodName = MethodName.reflectiveProxy("babar", Nil)

    assertError(
        classDef("A", superClass = Some(ObjectClass),
          methods = List(
            MethodDef(EMF.withNamespace(MemberNamespace.PublicStatic),
                babarMethodName, NON, Nil, AnyType, Some(int(1)))(EOH, UNV)
          )
        ),
        "reflective profixes are only allowed in the public namespace",
        allowReflectiveProxies = true
    )
  }

  @Test
  def noDuplicateVarDef(): Unit = {
    val body = Block(
      VarDef("x", NoOriginalName, IntType, mutable = false, int(1)),
      While(BooleanLiteral(true), {
        VarDef("x", NoOriginalName, IntType, mutable = false, int(1))
      })
    )

    assertError(
        classDef("A", kind = ClassKind.Interface, methods = List(mainMethodDef(body))),
        "Duplicate local variable name x."
    )
  }

  @Test
  def noDuplicateVarDefForIn(): Unit = {
    val body = Block(
      VarDef("x", NoOriginalName, IntType, mutable = false, int(1)),
      ForIn(JSObjectConstr(Nil), "x", NoOriginalName, Skip())
    )

    assertError(
        classDef("A", kind = ClassKind.Interface, methods = List(mainMethodDef(body))),
        "Duplicate local variable name x."
    )
  }

  @Test
  def noDuplicateVarDefTryCatch(): Unit = {
    val body = Block(
      VarDef("x", NoOriginalName, IntType, mutable = false, int(1)),
      TryCatch(Skip(), "x", NoOriginalName, Skip())(NoType)
    )

    assertError(
        classDef("A", kind = ClassKind.Interface, methods = List(mainMethodDef(body))),
        "Duplicate local variable name x."
    )
  }

  @Test
  def missingDelegateCtorCall(): Unit = {
    val ctorFlags = EMF.withNamespace(MemberNamespace.Constructor)

    assertError(
        classDef(
          "Foo", superClass = Some(ObjectClass),
          methods = List(
            MethodDef(ctorFlags, NoArgConstructorName, NON, Nil, NoType,
                Some(int(5)))(EOH, UNV)
          )
        ),
        "Constructor must contain a delegate constructor call")
  }

  @Test
  def invalidDelegateCtorCallTarget(): Unit = {
    val ctorFlags = EMF.withNamespace(MemberNamespace.Constructor)

    assertError(
        classDef(
          "Foo", superClass = Some(ObjectClass),
          methods = List(
            MethodDef(ctorFlags, NoArgConstructorName, NON, Nil, NoType, Some {
              ApplyStatically(EAF.withConstructor(true), thisFor("Foo"),
                  "Bar", NoArgConstructorName, Nil)(NoType)
            })(EOH, UNV)
          )
        ),
        "Invalid target class Bar for delegate constructor call; " +
        "expected Foo or java.lang.Object")
  }

  @Test
  def illegalCtorCall(): Unit = {
    val ctorFlags = EMF.withNamespace(MemberNamespace.Constructor)

    def ctorCall(receiver: Tree): ApplyStatically = {
      ApplyStatically(EAF.withConstructor(true), receiver,
          ObjectClass, NoArgConstructorName, Nil)(NoType)
    }

    val thiz = thisFor("Foo")

    assertError(
        classDef(
          "Foo", superClass = Some(ObjectClass),
          methods = List(
            MethodDef(ctorFlags, NoArgConstructorName, NON, Nil, NoType, Some(Block(
              ctorCall(thiz),
              ctorCall(Null())
            )))(EOH, UNV)
          )
        ),
        "Illegal constructor call")

    assertError(
        classDef(
          "Foo", superClass = Some(ObjectClass),
          methods = List(
            MethodDef(ctorFlags, NoArgConstructorName, NON, Nil, NoType, Some(Block(
              ctorCall(thiz),
              If(BooleanLiteral(true), ctorCall(thiz), Skip())(NoType)
            )))(EOH, UNV)
          )
        ),
        "Illegal constructor call")

    assertError(
        classDef(
          "Foo", superClass = Some(ObjectClass),
          methods = List(
            trivialCtor("Foo"),
            MethodDef(EMF, m("foo", Nil, V), NON, Nil, NoType, Some(Block(
              ctorCall(thiz)
            )))(EOH, UNV)
          )
        ),
        "Illegal constructor call")
  }

  @Test
  def thisType(): Unit = {
    def testThisTypeError(static: Boolean, expr: Tree, expectedMsg: String): Unit = {
      val methodFlags =
        if (static) EMF.withNamespace(MemberNamespace.PublicStatic)
        else EMF

      assertError(
          classDef(
            "Foo", superClass = Some(ObjectClass),
            methods = List(
              MethodDef(methodFlags, m("bar", Nil, V), NON, Nil, NoType, Some({
                consoleLog(expr)
              }))(EOH, UNV)
            )
          ),
          expectedMsg)
    }

    testThisTypeError(static = true,
        This()(NoType),
        "Cannot find `this` in scope")

    testThisTypeError(static = true,
        This()(ClassType("Foo", nullable = false)),
        "Cannot find `this` in scope")

    testThisTypeError(static = false,
        This()(NoType),
        "`this` of type Foo! typed as <notype>")

    testThisTypeError(static = false,
        This()(AnyType),
        "`this` of type Foo! typed as any")

    testThisTypeError(static = false,
        This()(AnyNotNullType),
        "`this` of type Foo! typed as any!")

    testThisTypeError(static = false,
        This()(ClassType("Bar", nullable = false)),
        "`this` of type Foo! typed as Bar!")

    testThisTypeError(static = false,
        This()(ClassType("Foo", nullable = true)),
        "`this` of type Foo! typed as Foo")

    testThisTypeError(static = false,
        Closure(arrow = true, Nil, Nil, None, This()(NoType), Nil),
        "Cannot find `this` in scope")

    testThisTypeError(static = false,
        Closure(arrow = true, Nil, Nil, None, This()(AnyType), Nil),
        "Cannot find `this` in scope")

    testThisTypeError(static = false,
        Closure(arrow = false, Nil, Nil, None, This()(NoType), Nil),
        "`this` of type any typed as <notype>")

    testThisTypeError(static = false,
        Closure(arrow = false, Nil, Nil, None, This()(ClassType("Foo", nullable = false)), Nil),
        "`this` of type any typed as Foo!")
  }

  @Test
  def restrictedThis(): Unit = {
    val xParamDef = paramDef("x", IntType)
    val xFieldName = FieldName("Foo", "x")

    def testRestrictedThisError(ctorStats: Tree*): Unit = {
      val ctorFlags = EMF.withNamespace(MemberNamespace.Constructor)

      assertError(
          classDef(
            "Foo", superClass = Some(ObjectClass),
            methods = List(
              MethodDef(ctorFlags, MethodName.constructor(List(I)), NON,
                  List(xParamDef), NoType, Some(Block(ctorStats: _*)))(EOH, UNV)
            )
          ),
          "Restricted use of `this` before the super constructor call")
    }

    val superCtorCall = trivialSuperCtorCall("Foo")
    val thiz = thisFor("Foo")

    testRestrictedThisError(
      thiz,
      superCtorCall
    )

    testRestrictedThisError(
      Select(thiz, xFieldName)(IntType),
      superCtorCall
    )

    testRestrictedThisError(
      Assign(Select(Select(thiz, xFieldName)(IntType), xFieldName)(IntType), int(5)),
      superCtorCall
    )

    testRestrictedThisError(
      Assign(Select(thiz, xFieldName)(IntType), Select(thiz, xFieldName)(IntType)),
      superCtorCall
    )

    testRestrictedThisError(
      ApplyStatically(EAF.withConstructor(true), thiz, ObjectClass,
          MethodIdent(MethodName.constructor(List(O))),
          List(thiz))(NoType)
    )
  }

  @Test
  def storeModule(): Unit = {
    val ctorFlags = EMF.withNamespace(MemberNamespace.Constructor)

    val superCtorCall = trivialSuperCtorCall("Foo")

    assertError(
      classDef(
        "Foo",
        kind = ClassKind.Class,
        superClass = Some(ObjectClass),
        methods = List(
          MethodDef(ctorFlags, NoArgConstructorName, NON, Nil, NoType, Some {
            Block(
              superCtorCall,
              StoreModule()
            )
          })(EOH, UNV)
        )
      ),
      "Illegal StoreModule"
    )

    assertError(
      classDef(
        "Foo",
        kind = ClassKind.ModuleClass,
        superClass = Some(ObjectClass),
        methods = List(
          MethodDef(ctorFlags, NoArgConstructorName, NON, Nil, NoType, Some {
            Block(
              StoreModule(),
              superCtorCall
            )
          })(EOH, UNV)
        )
      ),
      "Illegal StoreModule"
    )

    assertError(
      classDef(
        "Foo",
        kind = ClassKind.ModuleClass,
        superClass = Some(ObjectClass),
        methods = List(
          MethodDef(ctorFlags, NoArgConstructorName, NON, Nil, NoType, Some {
            Block(
              superCtorCall,
              If(BooleanLiteral(true), StoreModule(), Skip())(NoType)
            )
          })(EOH, UNV)
        )
      ),
      "Illegal StoreModule"
    )

    assertError(
      classDef(
        "Foo",
        kind = ClassKind.ModuleClass,
        superClass = Some(ObjectClass),
        methods = List(
          trivialCtor("Foo"),
          MethodDef(EMF, MethodName("foo", Nil, VoidRef), NON, Nil, NoType, Some {
            Block(
              StoreModule()
            )
          })(EOH, UNV)
        )
      ),
      "Illegal StoreModule"
    )

    assertError(
      classDef(
        "Foo",
        kind = ClassKind.JSModuleClass,
        superClass = Some("scala.scalajs.js.Object"),
        jsConstructor = Some(
          JSConstructorDef(JSCtorFlags, Nil, None,
              JSConstructorBody(StoreModule() :: Nil, JSSuperConstructorCall(Nil), Undefined() :: Nil))(
              EOH, UNV)
        )
      ),
      "Illegal StoreModule"
    )

    assertError(
      classDef(
        "Foo",
        kind = ClassKind.JSModuleClass,
        superClass = Some("scala.scalajs.js.Object"),
        jsConstructor = Some(
          JSConstructorDef(JSCtorFlags, Nil, None,
              JSConstructorBody(Nil, JSSuperConstructorCall(Nil),
                  If(BooleanLiteral(true), StoreModule(), Skip())(NoType) :: Undefined() :: Nil))(
              EOH, UNV)
        )
      ),
      "Illegal StoreModule"
    )
  }

  @Test
  def isAsInstanceOf(): Unit = {
    def testIsInstanceOfError(testType: Type): Unit = {
      assertError(
          mainTestClassDef(IsInstanceOf(int(5), testType)),
          s"${testType.show()} is not a valid test type for IsInstanceOf")
    }

    def testAsInstanceOfError(targetType: Type): Unit = {
      assertError(
          mainTestClassDef(AsInstanceOf(int(5), targetType)),
          s"${targetType.show()} is not a valid target type for AsInstanceOf")
    }

    def testIsAsInstanceOfError(tpe: Type): Unit = {
      testIsInstanceOfError(tpe)
      testAsInstanceOfError(tpe)
    }

    testIsAsInstanceOfError(NoType)
    testIsAsInstanceOfError(NullType)
    testIsAsInstanceOfError(NothingType)

    testIsAsInstanceOfError(
        RecordType(List(RecordType.Field("f", NON, IntType, mutable = false))))

    testIsInstanceOfError(AnyType)
    testAsInstanceOfError(AnyNotNullType)

    testIsInstanceOfError(ClassType(BoxedStringClass, nullable = true))
    testAsInstanceOfError(ClassType(BoxedStringClass, nullable = false))

    testIsInstanceOfError(ArrayType(ArrayTypeRef(IntRef, 1), nullable = true))
    testAsInstanceOfError(ArrayType(ArrayTypeRef(IntRef, 1), nullable = false))
  }
}

private object ClassDefCheckerTest {
  private def assertError(clazz: ClassDef, expectMsg: String,
      allowReflectiveProxies: Boolean = false, allowTransients: Boolean = false) = {
    var seen = false
    val reporter = new ErrorReporter {
      def reportError(msg: String)(implicit ctx: ErrorReporter.ErrorContext) = {
        assertEquals("unexpected error reported", expectMsg, msg)
        assertFalse("error reported multiple times", seen)
        seen = true
      }
    }

    new ClassDefChecker(clazz, allowReflectiveProxies, allowTransients, reporter).checkClassDef()
    assertTrue("no errors reported", seen)
  }
}
