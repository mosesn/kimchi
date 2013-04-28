package com.mosesn.kimchi.json

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class JsonDeserializerSpec extends FunSpec with ShouldMatchers {
  describe("JsonDeserializer") {
    it("should parse an empty json") {
      val empty = "{}"
      JsonDeserializer(empty) should be (JSON(Seq.empty))
    }

    it("should not parse an unclosed json") {
      val invalid = "{"
      val throwable = evaluating {JsonDeserializer(invalid)} should produce [Exception]
      throwable.getMessage should endWith ("Was not valid json serialized code")
    }

    it("should parse json recursively") {
      val recursive = "{\"OK\":{}}"
      JsonDeserializer(recursive) should be (JSON(Seq("\"OK\"" -> JSON(Seq.empty))))
    }

    it("should parse json with a few items") {
      val recursive = "{\"OK\":{}, \"NOK\":{}}"
      JsonDeserializer(recursive) should be (JSON(Seq("\"OK\"" -> JSON(Seq.empty), "\"NOK\"" -> JSON(Seq.empty))))
    }

    it("should not accept json with duplicate keys") {
      val recursive = "{\"OK\":{}, \"OK\":{}}"
      val throwable = evaluating {JsonDeserializer(recursive)} should produce [IllegalArgumentException]
      throwable.getMessage should endWith ("there are duplicate key names")
    }

    it("should parse json which has a string as a value") {
      val string = "{\"\": \"OKWHATEVER\"}"
      JsonDeserializer(string) should be (JSON(Seq("\"\"" -> JString("\"OKWHATEVER\""))))
    }

    it("should parse json which has a number as a value") {
      val number = "{\"\": 3.1}"
      JsonDeserializer(number) should be (JSON(Seq("\"\"" -> JNumber(3.1))))
    }

    it("should parse json which has a whole number as a value") {
      val number = "{\"\": 3}"
      JsonDeserializer(number) should be (JSON(Seq("\"\"" -> JNumber(3))))
    }

    it("should parse json which has a false as a value") {
      val jFalse = "{\"\": false}"
      JsonDeserializer(jFalse) should be (JSON(Seq("\"\"" -> JFalse)))
    }

    it("should parse json which has a true as a value") {
      val jTrue = "{\"\": true}"
      JsonDeserializer(jTrue) should be (JSON(Seq("\"\"" -> JTrue)))
    }

    it("should parse json which has a null as a value") {
      val jNull = "{\"\": null}"
      JsonDeserializer(jNull) should be (JSON(Seq("\"\"" -> JNull)))
    }

    it("should parse json which has a null as a value and spaces at the end") {
      val jNull = "{\"\": null }"
      JsonDeserializer(jNull) should be (JSON(Seq("\"\"" -> JNull)))
    }

    it("should parse json which has a null as a value and spaces at the beginning") {
      val jNull = "{ \"\": null}"
      JsonDeserializer(jNull) should be (JSON(Seq("\"\"" -> JNull)))
    }

    it("should parse json which has a null as a value and a space before :") {
      val jNull = "{\"\" : null}"
      JsonDeserializer(jNull) should be (JSON(Seq("\"\"" -> JNull)))
    }

    it("should parse json which has an empty array as a value") {
      val empty = "{\"\": []}"
      JsonDeserializer(empty) should be (JSON(Seq("\"\"" -> JArray(Seq.empty))))
    }

    it("should parse json which has a small array as a value") {
      val empty = "{\"\": [ \"\" ]}"
      JsonDeserializer(empty) should be (JSON(Seq("\"\"" -> JArray(Seq(JString("\"\""))))))
    }

    it("should parse json which has an array as a value") {
      val empty = "{\"\": [ \"\", 3, 4.1, null ]}"
      JsonDeserializer(empty) should be (JSON(Seq("\"\"" -> JArray(Seq(JString("\"\""), JNumber(3), JNumber(4.1), JNull)))))
    }
  }
}
