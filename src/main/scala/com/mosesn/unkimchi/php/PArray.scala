package com.mosesn.unkimchi.php

case class PArray(objects: Map[ValidKey, PObject]) extends PObject

object PArray {
  def empty = PArray(Map.empty)
}