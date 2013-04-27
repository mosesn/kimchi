package com.mosesn.kimchi.json

case class JSON(keyValues: Seq[(String, JType)]) extends JType
