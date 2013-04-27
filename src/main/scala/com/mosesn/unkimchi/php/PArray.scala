package com.mosesn.kimchi.php

case class PArray(objects: Seq[(ValidKey, PType)]) extends PType

object PArray {
  def empty = PArray(Seq.empty)
}
