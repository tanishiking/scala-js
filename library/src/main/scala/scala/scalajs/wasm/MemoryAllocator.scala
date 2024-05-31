package scala.scalajs.wasm

class MemoryAllocator {
  def allocate(size: Int): MemorySegment =
    throw new Error("stub") // body replaced by the compiler back-end
  def free(): Unit =
    throw new Error("stub") // body replaced by the compiler back-end
}

object MemoryAllocator {
  def withAllocator(block: MemoryAllocator => Unit): Unit = {
    val allocator = new MemoryAllocator()
    try {
      block(allocator)
    } finally {
      allocator.free()
    }
  }
}