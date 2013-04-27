package com.mosesn.kimchi.php

case class PArray(objects: Map[ValidKey, PType]) extends PType

object PArray {
  def empty = PArray(Map.empty)
}
