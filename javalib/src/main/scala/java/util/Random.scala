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

package java.util

import scala.annotation.tailrec

import scala.scalajs.js
import scala.scalajs.runtime.linkingInfo

class Random(seed_in: Long) extends AnyRef with java.io.Serializable {
  /* This class has two different implementations of seeding and computing
   * bits, depending on whether we are on Wasm on JS. On Wasm, we use the
   * implementation specified in the JavaDoc verbatim. On JS, however, that is
   * too slow, due to the use of `Long`s. Therefore, we decompose the
   * computations using 2x24 bits. See `nextJS()` for details.
   *
   * Because JS wants to separately store `seedHi` and `seedLo`, Wasm follows
   * suit (we do not want to penalize JS to help Wasm), although in Wasm they
   * really store 32 bits each.
   */

  private var seedHi: Int = _ // 24 (resp. 32) msb of the seed in JS (resp. Wasm)
  private var seedLo: Int = _ // 24 (resp. 32) lsb of the seed in JS (resp. Wasm)

  // see nextGaussian()
  private var nextNextGaussian: Double = _
  private var haveNextNextGaussian: Boolean = false

  setSeed(seed_in)

  def this() = this(Random.randomSeed())

  def setSeed(seed_in: Long): Unit = {
    val seed = ((seed_in ^ 0x5DEECE66DL) & ((1L << 48) - 1)) // as documented
    if (linkingInfo.isWebAssembly) {
      seedHi = (seed >>> 32).toInt
      seedLo = seed.toInt
    } else {
      seedHi = (seed >>> 24).toInt
      seedLo = seed.toInt & ((1 << 24) - 1)
    }
    haveNextNextGaussian = false
  }

  @noinline
  protected def next(bits: Int): Int =
    if (linkingInfo.isWebAssembly) nextWasm(bits)
    else nextJS(bits)

  @inline
  private def nextWasm(bits: Int): Int = {
    val oldSeed = (seedHi.toLong << 32) | (seedLo.toLong & 0xffffffffL)
    val newSeed = (oldSeed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1) // as specified
    seedHi = (newSeed >>> 32).toInt
    seedLo = newSeed.toInt

    (newSeed >>> (48 - bits)).toInt // as specified
  }

  @inline
  private def nextJS(bits: Int): Int = {
    /* This method is originally supposed to work with a Long seed from which
     * 48 bits are used.
     * Since Longs are too slow, we manually decompose the 48-bit seed in two
     * parts of 24 bits each.
     * The computation below is the translation in 24-by-24 bits of the
     * specified computation, taking care never to produce intermediate values
     * requiring more than 52 bits of precision.
     */

    @inline
    def rawToInt(x: Double): Int =
      (x.asInstanceOf[js.Dynamic] | 0.asInstanceOf[js.Dynamic]).asInstanceOf[Int]

    @inline
    def _24msbOf(x: Double): Int = rawToInt(x / (1 << 24).toDouble)

    @inline
    def _24lsbOf(x: Double): Int = rawToInt(x) & ((1 << 24) - 1)

    // seed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)

    val oldSeedHi = seedHi
    val oldSeedLo = seedLo

    val mul = 0x5DEECE66DL
    val mulHi = (mul >>> 24).toInt
    val mulLo = mul.toInt & ((1 << 24) - 1)

    val loProd = oldSeedLo.toDouble * mulLo.toDouble + 0xB
    val hiProd = oldSeedLo.toDouble * mulHi.toDouble + oldSeedHi.toDouble * mulLo.toDouble
    val newSeedHi =
      (_24msbOf(loProd) + _24lsbOf(hiProd)) & ((1 << 24) - 1)
    val newSeedLo =
      _24lsbOf(loProd)

    seedHi = newSeedHi
    seedLo = newSeedLo

    // (seed >>> (48 - bits)).toInt
    //   === ((seed >>> 16) >>> (32 - bits)).toInt because (bits <= 32)

    val result32 = (newSeedHi << 8) | (newSeedLo >> 16)
    result32 >>> (32 - bits)
  }

  def nextDouble(): Double = {
    // ((next(26).toLong << 27) + next(27)) / (1L << 53).toDouble
    ((next(26).toDouble * (1L << 27).toDouble) + next(27).toDouble) / (1L << 53).toDouble
  }

  def nextBoolean(): Boolean = next(1) != 0

  def nextInt(): Int = next(32)

  def nextInt(n: Int): Int = {
    if (n <= 0) {
      throw new IllegalArgumentException("n must be positive")
    } else if ((n & -n) == n) { // i.e., n is a power of 2
      /* The specification is
       *   ((n * next(31).toLong) >> 31).toInt
       *   == ((2**log2(n) * next(31).toLong) >> 31).toInt
       *   == ((next(31).toLong << log2(n)) >> 31).toInt
       *   == (next(31).toLong >> (31 - log2(n))).toInt
       *   == next(31) >> (31 - log2(n))
       * For a power of 2,
       *   log2(n) == numberOfTrailingZeros(n) == 31 - numberOfLeadingZeros(n)
       * hence, we simply get
       *   next(31) >> numberOfLeadingZeros(n)
       */
      next(31) >> Integer.numberOfLeadingZeros(n)
    } else {
      @tailrec
      def loop(): Int = {
        val bits = next(31)
        val value = bits % n
        if (bits - value + (n-1) < 0) loop()
        else value
      }

      loop()
    }
  }

  def nextLong(): Long = (next(32).toLong << 32) + next(32)

  def nextFloat(): Float = {
    // next(24).toFloat / (1 << 24).toFloat
    (next(24).toDouble / (1 << 24).toDouble).toFloat
  }

  def nextBytes(bytes: Array[Byte]): Unit = {
    var i = 0
    while (i < bytes.length) {
      var rnd = nextInt()
      var n = Math.min(bytes.length - i, 4)
      while (n > 0) {
        bytes(i) = rnd.toByte
        rnd >>= 8
        n -= 1
        i += 1
      }
    }
  }

  def nextGaussian(): Double = {
    // See http://www.protonfish.com/jslib/boxmuller.shtml

    /* The Box-Muller algorithm produces two random numbers at once. We save
     * the second one in `nextNextGaussian` to be used by the next call to
     * nextGaussian().
     */

    if (haveNextNextGaussian) {
      haveNextNextGaussian = false
      nextNextGaussian
    } else {
      var x, y, rds: Double = 0

      /* Get two random numbers from -1 to 1.
       * If the radius is zero or greater than 1, throw them out and pick two
       * new ones.
       * Rejection sampling throws away about 20% of the pairs.
       */
      do {
        x = nextDouble()*2-1
        y = nextDouble()*2-1
        rds = x*x + y*y
      } while (rds == 0 || rds > 1)

      val c = Math.sqrt(-2 * Math.log(rds) / rds)

      // Save y*c for next time
      nextNextGaussian = y*c
      haveNextNextGaussian = true

      // And return x*c
      x*c
    }
  }
}

object Random {

  /** Generate a random long from JS RNG to seed a new Random */
  private def randomSeed(): Long =
    (randomInt().toLong << 32) | (randomInt().toLong & 0xffffffffL)

  private def randomInt(): Int =
    (Math.floor(js.Math.random() * 4294967296.0) - 2147483648.0).toInt

}
