package com.mosesn.kimchi.json

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.CharSequenceReader

object JsonDeserializer extends JavaTokenParsers {
  def apply(string: String): JType = phrase(json)(new CharSequenceReader(string)) match {
    case Success(result, next) => result
    case _ => throw new Exception("Was not valid json serialized code")
  }

  private[this] lazy val json: Parser[JSON] = elem('{') ~> repsep(keyValue, ',') <~ elem('}') ^^ {
    JSON(_)
  }

  private[this] lazy val keyValue: Parser[(String, JType)] = stringLiteral ~ value ^^ {
    case first ~ second => (first, second)
  }

  private[this] lazy val value: Parser[JType] = json | array | string | number | boolean | jNull

  private[this] lazy val array: Parser[JArray] = repsep(value, ',') ^^ (JArray(_))

  private[this] lazy val string: Parser[JString] = stringLiteral ^^ (JString(_))

  private[this] lazy val number: Parser[JNumber] = whole | decimal

  private[this] lazy val decimal: Parser[JDouble] = decimalNumber ^^ { num =>
    JDouble(num.toDouble)
  }

  private[this] lazy val whole: Parser[JInt] = wholeNumber ^^ { num =>
    JInt(num.toInt)
  }

  private[this] lazy val boolean: Parser[JBoolean] = t | f

  private[this] lazy val t: Parser[JTrue.type] = "true" ^^ (_ => JTrue)

  private[this] lazy val f: Parser[JFalse.type] = "false" ^^ (_ => JFalse)

  private[this] lazy val jNull: Parser[JNull.type] = "null" ^^ (_ => JNull)

}
