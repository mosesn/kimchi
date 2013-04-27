package com.mosesn.kimchi.php

case class PBoolean(bool: Boolean) extends PObject

object PBoolean {
  def apply(num: Int): PBoolean = num match {
    case 0 => PBoolean(false)
    case 1 => PBoolean(true)
    case _ => throw new IllegalArgumentException("You handed me the wrong value")
  }
}
