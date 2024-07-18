/* Scala.js example code
 * Public domain
 * @author  Sébastien Doeraene
 */

package helloworld

import scala.scalajs.js
import js.annotation._

object HelloWorld {
  def main(args: Array[String]): Unit = {
    import js.DynamicImplicits.truthValue

    if (js.typeOf(js.Dynamic.global.document) != "undefined" &&
        js.Dynamic.global.document &&
        js.Dynamic.global.document.getElementById("playground")) {
      sayHelloFromDOM()
      sayHelloFromTypedDOM()
      sayHelloFromJQuery()
      sayHelloFromTypedJQuery()
    } else {
      println("Hello world!")
      NestedJS.localJSClassBasics()
    }
  }

  def sayHelloFromDOM(): Unit = {
    val document = js.Dynamic.global.document
    val playground = document.getElementById("playground")

    val newP = document.createElement("p")
    newP.innerHTML = "Hello world! <i>-- DOM</i>"
    playground.appendChild(newP)
  }

  def sayHelloFromTypedDOM(): Unit = {
    val document = window.document
    val playground = document.getElementById("playground")

    val newP = document.createElement("p")
    newP.innerHTML = "Hello world! <i>-- typed DOM</i>"
    playground.appendChild(newP)
  }

  def sayHelloFromJQuery(): Unit = {
    // val $ is fine too, but not very recommended in Scala code
    val jQuery = js.Dynamic.global.jQuery
    val newP = jQuery("<p>").html("Hello world! <i>-- jQuery</i>")
    newP.appendTo(jQuery("#playground"))
  }

  def sayHelloFromTypedJQuery(): Unit = {
    val jQuery = helloworld.JQuery
    val newP = jQuery("<p>").html("Hello world! <i>-- typed jQuery</i>")
    newP.appendTo(jQuery("#playground"))
  }
}

@js.native
@JSGlobalScope
object window extends js.Object {
  val document: DOMDocument = js.native

  def alert(msg: String): Unit = js.native
}

@js.native
trait DOMDocument extends js.Object {
  def getElementById(id: String): DOMElement = js.native
  def createElement(tag: String): DOMElement = js.native
}

@js.native
trait DOMElement extends js.Object {
  var innerHTML: String = js.native

  def appendChild(child: DOMElement): Unit = js.native
}

@js.native
@JSGlobal("jQuery")
object JQuery extends js.Object {
  def apply(selector: String): JQuery = js.native
}

@js.native
trait JQuery extends js.Object {
  def text(value: String): JQuery = js.native
  def text(): String = js.native

  def html(value: String): JQuery = js.native
  def html(): String = js.native

  def appendTo(parent: JQuery): JQuery = js.native
}

object NestedJS {
  def assertSame(x: Any, y: Any) = x == y
  def assertEquals(x: Any, y: Any) = x == y
  def assertTrue(x: Boolean) = x
  def assertFalse(x: Boolean) = !x
  /*
  def innerJSClassBasics(): Unit = {
    val container1 = new ScalaClassContainer("hello")

    val innerJSClass = container1.getInnerJSClass
    assertSame(innerJSClass, container1.getInnerJSClass)
    assertSame(innerJSClass, js.constructorOf[container1.InnerJSClass])
    assertEquals("function", js.typeOf(innerJSClass))

    val inner1 = new container1.InnerJSClass("world1")
    assertEquals("helloworld1", inner1.zzz)
    assertEquals("helloworld1foo", inner1.foo("foo"))
    assertTrue(inner1.isInstanceOf[container1.InnerJSClass])
    assertTrue(js.special.instanceof(inner1, innerJSClass))

    val inner2 = js.Dynamic.newInstance(innerJSClass)("world2")
    assertEquals("helloworld2", inner2.zzz)
    assertEquals("helloworld2foo", inner2.foo("foo"))
    assertTrue(inner2.isInstanceOf[container1.InnerJSClass])
    assertTrue(js.special.instanceof(inner2, innerJSClass))

    val container2 = new ScalaClassContainer("hi")
    val innerJSClass2 = container2.getInnerJSClass
    assertSame(innerJSClass, innerJSClass2)

    val inner3 = new container2.InnerJSClass("world3")
    assertEquals("hiworld3", inner3.zzz)
    assertEquals("hiworld3foo", inner3.foo("foo"))
    assertTrue(inner3.isInstanceOf[container2.InnerJSClass])
    assertTrue(js.special.instanceof(inner3, container2.getInnerJSClass))

    assertFalse(inner3.isInstanceOf[container1.InnerJSClass])
    assertFalse(js.special.instanceof(inner3, innerJSClass))
  }
  */
  def localJSClassBasics(): Unit = {
    val container1 = new ScalaClassContainer("hello")
    val localJSClass1 = container1.makeLocalJSClass("wide1")
    assertEquals("function", js.typeOf(localJSClass1))

    val inner1 = js.Dynamic.newInstance(localJSClass1)("world1")
    assertEquals("hellowide1world1", inner1.zzz)
    assertEquals("hellowide1world1foo", inner1.foo("foo"))
    assertTrue(js.special.instanceof(inner1, localJSClass1))
    assertFalse(inner1.isInstanceOf[container1.InnerJSClass])

    val inner2 = js.Dynamic.newInstance(localJSClass1)("world2")
    assertEquals("hellowide1world2", inner2.zzz)
    assertEquals("hellowide1world2foo", inner2.foo("foo"))

    val localJSClass2 = container1.makeLocalJSClass("wide2")
    assertSame(localJSClass1, localJSClass2)

    val inner3 = js.Dynamic.newInstance(localJSClass2)("world3")
    assertEquals("hellowide2world3", inner3.zzz)
    assertEquals("hellowide2world3foo", inner3.foo("foo"))
    assertTrue(js.special.instanceof(inner3, localJSClass2))
    assertFalse(js.special.instanceof(inner3, localJSClass1))
    assertFalse(inner3.isInstanceOf[container1.InnerJSClass])
  }

  class ScalaClassContainer(xxx: String) {
    class InnerJSClass(yyy: String) extends js.Object {
      val zzz: String = xxx + yyy

      def foo(a: String): String = xxx + yyy + a
    }

    def getInnerJSClass: js.Dynamic =
      js.constructorOf[InnerJSClass]

    def makeLocalJSClass(yyy: String): js.Dynamic = {
      class LocalJSClass(abc: String) extends js.Object {
        val zzz: String = xxx + yyy + abc

        def foo(a: String): String = xxx + yyy + abc + a
      }

      js.constructorOf[LocalJSClass]
    }

    var moduleSideEffect = 0

    class InnerJSClassDefaultParams_Issue4465(withDefault: String = "inner")(
        dependentDefault: String = withDefault) extends js.Object {
      def this(x: Int) = this(x.toString)()

      def foo(methodDefault: String = "foo"): String =
        s"$xxx $withDefault $dependentDefault $methodDefault"
    }

    object InnerJSClassDefaultParams_Issue4465 {
      moduleSideEffect += 1
    }

    class InnerJSClassDefaultParamsPrivateCompanion_Issue4526(
        withDefault: String = "inner") extends js.Object {
      def foo(methodDefault: String = "foo"): String =
        s"$xxx $withDefault $methodDefault"
    }

    private object InnerJSClassDefaultParamsPrivateCompanion_Issue4526
  }
}