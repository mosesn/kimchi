package com.mosesn.kimchi.php

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.CharSequenceReader

object PhpDeserializer extends JavaTokenParsers {
  def apply(string: String): PObject = phrase(pObject)(new CharSequenceReader(string)) match {
    case Success(result, next) => result
    case _ => throw new Exception("Was not valid PHP serialized code")
  }

  private[this] lazy val pObject: Parser[PObject] = array | pInt | pBoolean | pString | pFloat | pNull

  private[this] lazy val array: Parser[PArray] = arrayPrefix into unverifiedArray

  private[this] lazy val arrayPrefix: Parser[Int] = "a:" ~> wholeNumber <~ elem(':') ^^ { _.toInt }

  private[this] lazy val unverifiedArray: Int => Parser[PArray] = { num =>
    "{" ~> rep(keyValue) <~ "}" ^^ { items =>
      val arr = PArray(items.toMap)
      if (arr.objects.size != num)
        throw new Exception("invalid php array expected array of size %d got one of size %d" format (num, arr.objects.size))
      arr
    }
  }

  private[this] lazy val keyValue: Parser[(ValidKey, PObject)] = validKey ~ pObject ^^ {
    case first ~ second => (first, second)
  }

  private[this] lazy val validKey: Parser[ValidKey] = pInt | pString

  private[this] lazy val pBoolean: Parser[PBoolean] = elem('b') ~> elem(':') ~> wholeNumber <~ elem(';') ^^ { num =>
    PBoolean(num.toInt)
  }

  private[this] lazy val pInt: Parser[PInt] = elem('i') ~> elem(':') ~> wholeNumber <~ elem(';') ^^ { num =>
    PInt(num.toInt)
  }

  private[this] lazy val pString: Parser[PString] = stringPrefix into unverifiedString

  private[this] lazy val stringPrefix: Parser[Int] = elem('s') ~> elem(':') ~> wholeNumber<~ elem(':') ^^ (_.toInt)

  private[this] lazy val unverifiedString: Int => Parser[PString] = { num =>
    elem('\\') ~> elem('"') ~> string(num) <~ elem('\\') <~ elem('"') <~ elem(';') ^^ (PString(_))
  }

  private[this] lazy val string: Int => Parser[String] = { num =>
    new Parser[String] {
      override def apply(input: Input): ParseResult[String] = {
        Success(starting(num, input), input.drop(num))
      }

      private[this] def starting(num: Int, reader: Input): String =
        if (num > 0) reader.first +: starting(num - 1, reader.rest) else ""
    }
  }

  private[this] lazy val pFloat: Parser[PFloat] = elem('d') ~> elem(':') ~> decimalNumber <~ elem(';') ^^ { dec =>
    PFloat(dec.toDouble)
  }

  private[this] lazy val pNull: Parser[PNull.type] = elem('N') ~> elem(';') ^^ (_ => PNull)
}
