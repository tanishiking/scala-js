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

import java.lang.Cloneable
import java.lang.Utils._

import java.util.ScalaOps._

import scala.scalajs.js
import scala.scalajs.LinkingInfo

class ArrayDeque[E] private (initialCapacity: Int)
    extends AbstractCollection[E] with Deque[E] with Cloneable with Serializable {
  self =>

  /* This class has two different implementations for the internal storage.
   * depending on whether we are on Wasm or JS.
   * On JS, we utilize `js.Array`. On Wasm, for performance reasons,
   * we use a scala.Array to avoid JS interop.
   */

  private var inner: Array[AnyRef] = new Array[AnyRef](Math.max(initialCapacity, 16))

  private var status = 0
  private var startIndex = 0 // inclusive, 0 <= startIndex < inner.length
  private var endIndex = // exclusive, 0 < endIndex <= inner.length
    inner.length
  private var empty = true

  def this() = this(16)

  def this(c: Collection[_ <: E]) = {
    this(c.size())
    addAll(c)
  }

  @inline
  override def isEmpty(): Boolean = empty

  def addFirst(e: E): Unit =
    offerFirst(e)

  def addLast(e: E): Unit =
    offerLast(e)

  def offerFirst(e: E): Boolean = {
    if (e == null) {
      throw new NullPointerException()
    } else {
      ensureCapacityForAdd()
      startIndex -= 1
      if (startIndex < 0)
        startIndex = length() - 1
      inner(startIndex) = e.asInstanceOf[AnyRef]
      status += 1
      empty = false
      true
    }
  }

  def offerLast(e: E): Boolean = {
    if (e == null) {
      throw new NullPointerException()
    } else {
      ensureCapacityForAdd()
      endIndex += 1
      if (endIndex > length())
        endIndex = 1
      inner(endIndex - 1) = e.asInstanceOf[AnyRef]
      status += 1
      empty = false
      true
    }
  }

  def removeFirst(): E = {
    if (isEmpty())
      throw new NoSuchElementException()
    else
      pollFirst()
  }

  def removeLast(): E = {
    if (isEmpty())
      throw new NoSuchElementException()
    else
      pollLast()
  }

  def pollFirst(): E = {
    if (isEmpty()) null.asInstanceOf[E]
    else {
      val res = inner(startIndex).asInstanceOf[E]
      inner(startIndex) = null // free reference for GC
      startIndex += 1
      if (startIndex == endIndex)
        empty = true
      if (startIndex >= length())
        startIndex = 0
      status += 1
      res
    }
  }

  def pollLast(): E = {
    if (isEmpty()) {
      null.asInstanceOf[E]
    } else {
      val res = inner(endIndex - 1).asInstanceOf[E]
      inner(endIndex - 1) = null // free reference for GC
      endIndex -= 1
      if (startIndex == endIndex)
        empty = true
      if (endIndex <= 0)
        endIndex = length()
      status += 1
      res
    }
  }

  def getFirst(): E = {
    if (isEmpty())
      throw new NoSuchElementException()
    else
      peekFirst()
  }

  def getLast(): E = {
    if (isEmpty())
      throw new NoSuchElementException()
    else
      peekLast()
  }

  def peekFirst(): E = {
    if (isEmpty()) {
      null.asInstanceOf[E]
    } else {
      inner(startIndex).asInstanceOf[E]
    }
  }

  def peekLast(): E = {
    if (isEmpty()) {
      null.asInstanceOf[E]
    } else {
      inner(endIndex - 1).asInstanceOf[E]
    }
  }

  def removeFirstOccurrence(o: Any): Boolean = {
    val i = firstIndexOf(o)
    if (i == -1) {
      false
    } else {
      removeAt(i)
      true
    }
  }

  def removeLastOccurrence(o: Any): Boolean = {
    val i = lastIndexOf(o)
    if (i == -1) {
      false
    } else {
      removeAt(i)
      true
    }
  }

  override def add(e: E): Boolean = {
    addLast(e)
    true
  }

  def offer(e: E): Boolean = offerLast(e)

  def remove(): E = removeFirst()

  def poll(): E = pollFirst()

  def element(): E = getFirst()

  def peek(): E = peekFirst()

  def push(e: E): Unit = addFirst(e)

  def pop(): E = removeFirst()

  def size(): Int = {
    if (isEmpty()) 0
    else if (endIndex > startIndex) endIndex - startIndex
    else (endIndex + length()) - startIndex
  }

  def iterator(): Iterator[E] = new Iterator[E] {
    private def checkStatus() = {
      if (self.status != expectedStatus)
        throw new ConcurrentModificationException()
    }

    private var expectedStatus = self.status

    private var lastIndex: Int = -1
    private var nextIndex: Int =
      if (isEmpty()) -1
      else startIndex

    def hasNext(): Boolean = {
      checkStatus()
      nextIndex != -1
    }

    def next(): E = {
      if (!hasNext()) // also checks status
        throw new NoSuchElementException()

      lastIndex = nextIndex

      nextIndex += 1
      if (nextIndex == endIndex)
        nextIndex = -1
      else if (nextIndex >= length())
        nextIndex = 0

      inner(lastIndex).asInstanceOf[E]
    }

    override def remove(): Unit = {
      checkStatus()
      if (lastIndex == -1)
        throw new IllegalStateException()

      val laterShifted = removeAt(lastIndex)
      lastIndex = -1
      expectedStatus = self.status

      if (laterShifted && nextIndex != -1) {
        /* assert(nextIndex != 0)
         * Why? Assume nextIndex == 0, that means the element we just removed
         * was at the end of the ring-buffer. But in this case, removeAt shifts
         * forward to avoid copying over the buffer boundary.
         * Therefore, laterShifted cannot be true.
         */
        nextIndex -= 1
      }
    }
  }

  def descendingIterator(): Iterator[E] = new Iterator[E] {
    private def checkStatus() = {
      if (self.status != expectedStatus)
        throw new ConcurrentModificationException()
    }

    private var expectedStatus = self.status

    private var lastIndex: Int = -1
    private var nextIndex: Int =
      if (isEmpty()) -1
      else endIndex - 1

    def hasNext(): Boolean = {
      checkStatus()
      nextIndex != -1
    }

    def next(): E = {
      if (!hasNext()) // also checks status
          throw new NoSuchElementException()

      lastIndex = nextIndex

      if (nextIndex == startIndex) {
        nextIndex = -1
      } else {
        nextIndex -= 1
        if (nextIndex < 0)
          nextIndex = length() - 1
      }

      inner(lastIndex).asInstanceOf[E]
    }

    override def remove(): Unit = {
      checkStatus()
      if (lastIndex == -1)
          throw new IllegalStateException()


      val laterShifted = removeAt(lastIndex)
      expectedStatus = self.status
      lastIndex = -1

      if (!laterShifted && nextIndex != -1) {
        /* assert(nextIndex < inner.length - 1)
         * Why? Assume nextIndex == inner.length - 1, that means the element we
         * just removed was at the beginning of the ring buffer (recall, this is
         * a backwards iterator). However, in this case, removeAt would shift
         * the next elements (in forward iteration order) backwards.
         * That implies laterShifted, so we would not hit this branch.
         */
        nextIndex += 1
      }
    }
  }

  override def contains(o: Any): Boolean = firstIndexOf(o) != -1

  override def remove(o: Any): Boolean = removeFirstOccurrence(o)

  override def clear(): Unit = {
    if (!isEmpty())
      status += 1
    empty = true
    startIndex = 0
    endIndex = length()
  }

  private def firstIndexOf(o: Any): Int = {
    // scalastyle:off return
    if (isEmpty())
      return -1
    val inner = this.inner // local copy
    val capacity = length() // local copy
    val endIndex = this.endIndex // local copy
    var i = startIndex
    do {
      if (i >= capacity)
        i = 0
      val obj = inner(i).asInstanceOf[E]
      if (Objects.equals(obj, o))
        return i
      i += 1 // let i overrun so we catch endIndex == capacity
    } while (i != endIndex)
    -1
    // scalastyle:on return
  }

  private def lastIndexOf(o: Any): Int = {
    // scalastyle:off return
    if (isEmpty())
      return -1
    val inner = this.inner // local copy
    val startIndex = this.startIndex // local copy
    var i = endIndex
    do {
      i -= 1
      if (i < 0)
        i = length() - 1
      val obj = inner(i).asInstanceOf[E]
      if (Objects.equals(obj, o))
        return i
    } while (i != startIndex)
    -1
    // scalastyle:on return
  }

  private def ensureCapacityForAdd(): Unit = {
    if (isEmpty()) {
      // Nothing to do (constructor ensures capacity is always non-zero).
    } else if (startIndex == 0 && endIndex == length()) {
      val oldCapacity = length()
      inner = Arrays.copyOf(inner, oldCapacity * 2)
    } else if (startIndex == endIndex) {
      val oldCapacity = length()
      val newArr = new Array[AnyRef](oldCapacity * 2)
      System.arraycopy(inner, 0, newArr, oldCapacity, endIndex)
      inner= newArr
      endIndex += oldCapacity
    }
  }

  /* Removes the element at index [[target]]
   *
   * The return value indicates which end of the queue was shifted onto the
   * element to be removed.
   *
   * @returns true if elements after target were shifted onto target or target
   *     was the last element. Returns false, if elements before target were
   *     shifted onto target or target was the first element.
   */
  private def removeAt(target: Int): Boolean = {
    /* Note that if size == 1, we always take the first branch.
     * Therefore, we need not handle the empty flag in this method.
     */

    if (target == startIndex) {
      pollFirst()
      false
    } else if (target == endIndex - 1) {
      pollLast()
      true
    } else if (target < endIndex) {
      System.arraycopy(inner, target + 1, inner, target, endIndex - target)
      inner(endIndex - 1) = null // free reference for GC
      status += 1

      /* Note that endIndex >= 2:
       * By previous if: target < endIndex
       * ==> target <= endIndex - 1
       * By previous if: target < endIndex - 1 (non-equality)
       * ==> target <= endIndex - 2
       * By precondition: target >= 0
       * ==> 0 <= endIndex - 2
       * ==> endIndex >= 2
       *
       * Therefore we do not need to perform an underflow check.
       */
      endIndex -= 1

      true
    } else {
      // Shift elements from startIndex towards target

      /* Note that target > startIndex.
       * Why? Assume by contradiction: target <= startIndex
       * By previous if: target >= endIndex.
       * By previous if: target < startIndex (non-equality)
       * ==> endIndex <= target < startIndex.
       * ==> target is not in the active region of the ringbuffer.
       * ==> contradiction.
       */

      System.arraycopy(inner, startIndex, inner, startIndex + 1, target - startIndex)
      inner(startIndex) = null // free reference for GC

      status += 1

      /* Note that startIndex <= inner.length - 2:
       * By previous proof: target > startIndex
       * By precondition: target <= inner.length - 1
       * ==> startIndex < inner.length - 1
       * ==> startIndex <= inner.length - 2
       *
       * Therefore we do not need to perform an overflow check.
       */
      startIndex += 1
      false
    }
  }

  @inline
  private def length(): Int =
    inner.length
}
