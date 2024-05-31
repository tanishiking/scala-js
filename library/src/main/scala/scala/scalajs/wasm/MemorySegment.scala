package scala.scalajs.wasm

class MemorySegment(val start: Int, val size: Int) {
  def getByte(offset: Int): Byte = {
    validate(offset, 1)
    _loadByte(offset + start)
  }
  def getInt(offset: Int): Int = {
    validate(offset, 4)
    _loadInt(offset + start)
  }

  def setByte(offset: Int, value: Byte): Unit = {
    validate(offset, 1)
    _storeByte(offset + start, value)
  }
  def setInt(offset: Int, value: Int): Unit = {
    validate(offset, 4)
    _storeInt(offset + start, value)
  }

  private def validate(offset: Int, requiredBytes: Int): Unit = {
    require(
      offset + requiredBytes >= 0 && offset + requiredBytes <= size,
      s"MemorySegment.validate($requiredBytes) failed, can't available $requiredBytes bytes"
    )
  }

  // body replaced by the compiler back-end
  private def _loadByte(offset: Int): Byte = throw new Error("stub")
  private def _loadInt(offset: Int): Int = throw new Error("stub")
  private def _storeByte(offset: Int, value: Byte): Unit = throw new Error("stub")
  private def _storeInt(offset: Int, value: Int): Unit = throw new Error("stub")
}
