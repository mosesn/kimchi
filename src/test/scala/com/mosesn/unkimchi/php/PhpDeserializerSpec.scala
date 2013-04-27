package com.mosesn.kimchi.php

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import scala.util.Random

class PhpDeserializerSpec extends FunSpec with ShouldMatchers {
  val rand = new Random()
  describe("PhpDeserializer") {
    val empty = """a:0:{}"""
    it("should deserialize a valid empty array") {
      val expected = PArray.empty
      val actual = PhpDeserializer(empty)
      actual should be (expected)
    }

    it("should fail to deserialize an invalid empty array") {
      val x = rand.nextInt()
      val invalidEmpty = "a:%d:{}".format(x)
      val throwable = evaluating {PhpDeserializer(invalidEmpty)} should produce [Exception]
      throwable.getMessage() should endWith ("invalid php array expected array of size %d got one of size 0" format x)
    }

    it("should deserialize recursively") {
      val recursive = "a:1:{i:0;a:0:{}}"
      val expected = PArray(Seq(PInt(0) -> PArray.empty))
      val actual = PhpDeserializer(recursive)
      actual should be (expected)
    }

    it("should deserialize numbers") {
      val x = rand.nextInt()
      val number = "i:%d;" format x
      val expected = PInt(x)
      val actual = PhpDeserializer(number)
      actual should be (expected)
    }

    it("should deserialize valid booleans") {
      val tru = "b:1;"
      val fals = "b:0;"
      val expectedTrue = PBoolean(true)
      val expectedFalse = PBoolean(false)
      PhpDeserializer(tru) should be (expectedTrue)
      PhpDeserializer(fals) should be (expectedFalse)
    }

    it("should fail on invalid booleans") {
      val invalidBool = "b:2;"
      val throwable = evaluating {PhpDeserializer(invalidBool)} should produce [IllegalArgumentException]
      throwable.getMessage() should endWith ("You handed me the wrong value")
    }

    it("should parse valid strings") {
      val empty = "s:0:\\\"\\\";"
      val string = "s:2:\\\"ab\\\";"
      val expectedEmpty = PString("")
      val expectedString = PString("ab")
      PhpDeserializer(empty) should be (expectedEmpty)
      PhpDeserializer(string) should be (expectedString)
    }

    it("should fail on invalid strings") {
      val invalid = "s:1:\\\"\\\";"
      val throwable = evaluating { PhpDeserializer(invalid) } should produce [Exception]
      throwable.getMessage() should endWith ("Was not valid PHP serialized code")
    }

    it("should work for string keys") {
      val array = "a:1:{s:0:\\\"\\\";a:0:{}}"
      PhpDeserializer(array) should be (PArray(Seq(PString("") -> PArray.empty)))
    }

    it("should parse a double") {
      val double = "d:3;"
      PhpDeserializer(double) should be (PFloat(3))
    }

    it("should parse a null") {
      val pNullString = "N;"
      PhpDeserializer(pNullString) should be (PNull)
    }

    it("should parse an empty object") {
      val oString = "O:7:\\\"MyClass\\\":0:{}"
      PhpDeserializer(oString) should be (new PObject("MyClass", Seq()))
    }

    it("should parse a simple object") {
      val oString = "O:7:\\\"MyClass\\\":1:{s:3:\\\"foo\\\";i:10;}"
      PhpDeserializer(oString) should be (new PObject("MyClass", Seq(PString("foo") -> PInt(10))))
    }

    it("should parse an object") {
      val oString = "O:7:\\\"MyClass\\\":2:{s:3:\\\"foo\\\";i:10;s:3:\\\"bar\\\";i:20;}"
      PhpDeserializer(oString) should be (new PObject("MyClass", Seq(PString("foo") -> PInt(10), PString("bar") -> PInt(20))))
    }

    it("should parse a recursive object") {
      val oString = "O:7:\\\"MyClass\\\":1:{s:3:\\\"foo\\\";r:1;}"
      PhpDeserializer(oString) should be (new PObject("MyClass", Seq(PString("foo") -> PRecursion(1))))
    }

    it("should parse a referentially recursive object") {
      val oString = "O:7:\\\"MyClass\\\":1:{s:3:\\\"foo\\\";R:1;}"
      PhpDeserializer(oString) should be (new PObject("MyClass", Seq(PString("foo") -> PReference(1))))
    }
  }
}
