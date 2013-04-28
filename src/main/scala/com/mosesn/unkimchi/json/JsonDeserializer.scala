package com.mosesn.kimchi.json

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.CharSequenceReader

object JsonDeserializer extends JavaTokenParsers {
  def apply(string: String): JType = phrase(json)(new CharSequenceReader(string)) match {
    case Success(result, next) => result
    case _ => throw new Exception("Was not valid json serialized code")
  }

  private[this] lazy val json: Parser[JSON] = "{" ~> repsep(keyValue, ",") <~ "}" ^^ { seq =>
    require((seq map (_._1)).distinct.size == seq.size, "there are duplicate key names")
    JSON(seq)
  }

  private[this] lazy val keyValue: Parser[(String, JType)] = key ~ value ^^ {
    case first ~ second => (first, second)
  }

  private[this] lazy val key: Parser[String] = stringLiteral <~ ":"

  private[this] lazy val value: Parser[JType] = json | array | string | number | boolean | jNull

  private[this] lazy val array: Parser[JArray] = "[" ~> repsep(value, ",") <~ "]" ^^ (JArray(_))

  private[this] lazy val string: Parser[JString] = stringLiteral ^^ (JString(_))

  private[this] lazy val number: Parser[JNumber] = decimalNumber ^^ { num =>
    JNumber(num.toDouble)
  }

  private[this] lazy val boolean: Parser[JBoolean] = t | f

  private[this] lazy val t: Parser[JTrue.type] = "true" ^^ (_ => JTrue)

  private[this] lazy val f: Parser[JFalse.type] = "false" ^^ (_ => JFalse)

  private[this] lazy val jNull: Parser[JNull.type] = "null" ^^ (_ => JNull)

}
