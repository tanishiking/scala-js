package test

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello from Scala.js full linking test!")

    // Test some basic Scala 3 features
    val list = List(1, 2, 3)
    val doubled = list.map(_ * 2)
    assert(doubled == List(2, 4, 6))

    println("All assertions passed!")
  }
}
