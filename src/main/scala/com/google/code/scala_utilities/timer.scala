package com.google.code.scala_utilities

object Timer {
  var start:Long = 0L
  var end:Long = 0L
  def go = {
    start = System.currentTimeMillis
  }
  def stop = {
    end = System.currentTimeMillis
    println(">   " + (end - start)/ 1000.0 + " s")
  }
}
