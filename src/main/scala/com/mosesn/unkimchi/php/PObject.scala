package com.mosesn.kimchi.php

case class PObject(className: String, fields: Seq[(PString, PType)]) extends PType
