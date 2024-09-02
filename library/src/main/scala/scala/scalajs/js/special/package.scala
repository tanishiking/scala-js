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

package scala.scalajs.js

import scala.scalajs.js
import scala.scalajs.js.annotation._

/** Contains special primitives of interoperability with JavaScript which are
 *  of limited importance or rare usefulness.
 *
 *  In theory, all of the members of this package could equally well be part of
 *  the `scala.scalajs.js` package. They are sligthly "hidden" in this
 *  `special` package so that they are not used on a daily basis, but only when
 *  absolutely necessary.
 *
 *  Everything in this package is a "I-know-what-I-am-doing" API. Notably, no
 *  attempt is made to guide the user with types that are not hard
 *  requirements.
 */
package object special {

  @js.native
  private trait FullyDynamicProps extends js.Object {
    @JSBracketAccess
    def update(prop: scala.Any, value: scala.Any): Unit = js.native
  }

  /** Constructs a new object with the specified properties.
   *
   *  This method is the exact equivalent of an object initializer in
   *  JavaScript (aka an object literal).
   *
   *  In most cases, you should use a new anonymous JS class if you have a
   *  typed API, or [[js.Dynamic.literal]] in a dynamically typed setting.
   */
  // intrinsic
  def objectLiteral(properties: (scala.Any, scala.Any)*): js.Object = {
    val result = new js.Object().asInstanceOf[FullyDynamicProps]
    for (pair <- properties)
      result(pair._1) = pair._2
    result
  }

  /** Tests whether two values are equal according to ECMAScript's
   *  *Strict Equality Comparison* (`===`).
   *
   *  This is equivalent to `x eq y`, except that:
   *
   *  - `strictEquals(NaN, NaN)` is `false` whereas `NaN eq NaN` is `true`
   *  - `strictEquals(+0.0, -0.0)` is `true` whereas `+0.0 eq -0.0` is `false`
   *
   *  @return
   *    the result of `x === y` where `===` is the ECMAScript operator.
   */
  def strictEquals(x: scala.Any, y: scala.Any): Boolean =
    throw new java.lang.Error("stub")

  /** Tests whether an object has a given enumerable property in its prototype
   *  chain.
   *
   *  This method is the exact equivalent of `p in o` in JavaScript.
   *
   *  The recommended surface syntax to perform `p in o` is to use
   *  `js.Object.hasProperty(o, p)`.
   */
  def in(p: scala.Any, o: scala.Any): Boolean =
    throw new java.lang.Error("stub")

  /** Dynamically tests whether a value is an instance of a JavaScript class.
   *
   *  This method is the exact equivalent of `x instanceof clazz` in
   *  JavaScript.
   *
   *  Using this method is only necessary when `clazz` is only known at
   *  run-time. In most cases, you should use
   *  {{{
   *  x.isInstanceOf[C]
   *  }}}
   *  instead, where `C` is the statically defined class corresponding to
   *  `clazz`. In particular, the following identity holds for all `x` and `C`:
   *  {{{
   *  x.isInstanceOf[C] == js.special.instanceof(x, js.constructorOf[C])
   *  }}}
   */
  def instanceof(x: scala.Any, clazz: scala.Any): Boolean =
    throw new java.lang.Error("stub")

  /** Deletes a property of an object.
   *
   *  This method is the exact equivalent of the `delete obj[key]` statement
   *  of JavaScript (and by extension of `delete obj.key` if `key` is a
   *  constant string).
   *
   *  The property must be configurable. Otherwise, this method throws a
   *  [[js.TypeError]].
   *
   *  Rather than using this method, it is often preferable to use a
   *  [[js.Dictionary]] and its `-=` method.
   */
  def delete(obj: scala.Any, key: scala.Any): Unit =
    throw new java.lang.Error("stub")

  /** Enumerates the keys of an object.
   *
   *  This method is the exact equivalent of the `for (a in o) {}` statement of
   *  JavaScript. A call of the form
   *  {{{
   *  forin(obj)(f)
   *  }}}
   *  corresponds to the JavaScript statement
   *  {{{
   *  const objTemp = obj;
   *  const fTemp = f;
   *  for (const x in objTemp) {
   *    fTemp(x)
   *  }
   *  }}}
   *
   *  `for..in` loops exhibit performance cliffs in most JavaScript engines,
   *  which means their performance is very dependent on their surroundings.
   *  Therefore, it is recommended to avoid this method and use
   *  [[js.Any.ObjectCompanionOps.properties js.Object.properties]] instead, if
   *  possible.
   */
  def forin(obj: scala.Any)(f: js.Function1[scala.Any, scala.Any]): Unit =
    throw new java.lang.Error("stub")

  /** Throw an arbitrary value, which will be caught as is by a JavaScript
   *  `try..catch` statement.
   *
   *  Usually, a Scala `throw` expression is more appropriate. Even if you
   *  want to throw a JS error type such as [[js.Error]], it is more idiomatic
   *  to wrap it in a [[js.JavaScriptException]] and throw that one.
   *
   *  However, if you hold a value of an arbitrary type, which was caught by a
   *  JavaScript `try..catch` statement (sometimes indirectly, such as with
   *  [[js.Promise]]s), it is appropriate to use `js.special.throw` to rethrow
   *  it.
   */
  def `throw`(ex: scala.Any): Nothing =
    throw new java.lang.Error("stub")

  /** Performs a JavaScript `try..catch`, which can catch any type of value.
   *
   *  Usually, a Scala `try..catch` expression catching [[Throwable]] is more
   *  appropriate. Values that are not instances of [[Throwable]], such as JS
   *  error values, are then wrapped in a [[js.JavaScriptException]].
   *
   *  However, if you need to get the originally thrown value, for example to
   *  pass it on to a JavaScript error handler, it is appropriate to use
   *  `js.special.tryCatch`.
   */
  def tryCatch[A](body: js.Function0[A])(handler: js.Function1[scala.Any, A]): A =
    throw new java.lang.Error("stub")

  /** Wrap any value so that it can be assigned to a [[Throwable]].
   *
   *  Instances of [[Throwable]] are returned as is. Other values are wrapped
   *  in a [[js.JavaScriptException]].
   */
  def wrapAsThrowable(ex: scala.Any): Throwable =
    throw new java.lang.Error("stub")

  /** Unwrap an exception value wrapped with `wrapAsThrowable`.
   *
   *  Instances of [[js.JavaScriptException]] are unwrapped to return the
   *  underlying value. Other values are returned as is.
   *
   *  @throws java.lang.NullPointerException
   *    If `th` is `null`. Subject to Undefined Behaviors.
   */
  def unwrapFromThrowable(th: Throwable): scala.Any =
    throw new java.lang.Error("stub")

  /** The value of the JavaScript `this` at the top-level of the generated
   *  file.
   *
   *  This returns the value that would be obtained by writing `this` at the
   *  top-level of the JavaScript file generated by Scala.js. In scripts, this
   *  is equivalent to the *global object*, but in other environments, it is
   *  not necessarily the case. For example, in CommonJS modules on Node.js,
   *  `this` is the object representing the `exports` of the current module,
   *  and it is `undefined` in ECMAScript modules.
   *
   *  Using this value should be rare, and mostly limited to writing code
   *  detecting what the global object is. For example, a typical detection
   *  code--in case we do not need to worry of ES modules--looks like:
   *  {{{
   *  val globalObject = {
   *    import js.Dynamic.{global => g}
   *    if (js.typeOf(g.global) != "undefined" && (g.global.Object eq g.Object)) {
   *      // Node.js environment detected
   *      g.global
   *    } else {
   *      // In all other well-known environment, we can use the global `this`
   *      js.special.fileLevelThis
   *    }
   *  }
   *  }}}
   *  Note that the above code is not comprehensive, as there can be JavaScript
   *  environments where the global object cannot be fetched neither through
   *  `global` nor `this`. If your code needs to run in such an environment, it
   *  is up to you to use an appropriate detection procedure.
   */
  @inline
  def fileLevelThis: scala.Any =
    throw new java.lang.Error("stub")

  /** Exact equivalent of the `debugger` keyword of JavaScript.
   *
   *  `debugger()` invokes any available debugging functionality.
   *  If no debugging functionality is available, this method has no effect.
   *
   *  MDN
   *
   *  Browser support:
   *  - Has no effect in Rhino nor, apparently, in Firefox
   *  - In Chrome, it has no effect unless the developer tools are opened
   *    beforehand.
   */
  def debugger(): Unit =
    throw new java.lang.Error("stub")
}
