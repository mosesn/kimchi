package com.mosesn.kimchi.php

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.CharSequenceReader

object PhpDeserializer extends JavaTokenParsers {
  def apply(string: String): PType = phrase(pType)(new CharSequenceReader(string)) match {
    case Success(result, next) => result
    case _ => throw new Exception("Was not valid PHP serialized code")
  }

  private[this] lazy val pType: Parser[PType] = array | pInt | pBoolean | pString | pFloat | pNull | pObject

  private[this] lazy val array: Parser[PArray] = arrayPrefix into unverifiedArray

  private[this] lazy val arrayPrefix: Parser[Int] = elem('a') ~> numberPrefix

  private[this] lazy val unverifiedArray: Int => Parser[PArray] = { num =>
    "{" ~> rep(keyValue) <~ "}" ^^ { items =>
      require((items map (_._1)).distinct.size == items.size, "arrays cannot have duplicate keys")
      require(items.size == num, "invalid php array expected array of size %d got one of size %d" format (num, items.size))
      PArray(items)
    }
  }

  private[this] lazy val keyValue: Parser[(ValidKey, PType)] = validKey ~ pValue ^^ {
    case first ~ second => (first, second)
  }

  private[this] lazy val pValue: Parser[PType] = pType | pReference | pRecursion

  private[this] lazy val validKey: Parser[ValidKey] = pInt | pString

  private[this] lazy val pBoolean: Parser[PBoolean] = elem('b') ~> elem(':') ~> wholeNumber <~ elem(';') ^^ { num =>
    PBoolean(num.toInt)
  }

  private[this] lazy val pInt: Parser[PInt] = elem('i') ~> elem(':') ~> wholeNumber <~ elem(';') ^^ { num =>
    PInt(num.toInt)
  }

  private[this] lazy val pString: Parser[PString] = (stringPrefix into unverifiedString) <~ elem(';') ^^ (PString(_))

  private[this] lazy val stringPrefix: Parser[Int] = elem('s') ~> numberPrefix

  private[this] lazy val numberPrefix: Parser[Int] = elem(':') ~> wholeNumber <~ elem(':') ^^ (_.toInt)

  private[this] lazy val unverifiedString: Int => Parser[String] = { num =>
    elem('\\') ~> elem('"') ~> string(num) <~ elem('\\') <~ elem('"')
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

  private[this] lazy val pObject: Parser[PObject] = objectPrefix into unverifiedObject

  private[this] lazy val objectName: Parser[String] = elem('O') ~> numberPrefix into unverifiedString

  private[this] lazy val objectPrefix: Parser[(String, Int)] = objectName ~ objectSize ^^ {
    case first ~ second => (first, second)
  }

  private[this] lazy val objectSize: Parser[Int] = elem(':') ~> wholeNumber <~ elem(':') ^^ (_.toInt)

  private[this] lazy val unverifiedObject: ((String, Int)) => Parser[PObject] = {
    case (className, numArgs) => unnamedObject(numArgs) ^^ { seq =>
      require((seq map (_._1)).distinct.size == seq.size, "objects cannot have duplicate fields")
      PObject(className, seq)
    }
  }

  private[this] lazy val unnamedObject: Int => Parser[Seq[(PString, PType)]] = { num =>
    elem('{') ~> rep(stringValue) <~ elem('}') ^^ { seq =>
      require(seq.size == num, "the object needs to the same length as specified")
      seq
    }
  }

  private[this] lazy val stringValue: Parser[(PString, PType)] = pString ~ pValue ^^ {
    case first ~ second => (first, second)
  }

  private[this] lazy val pRecursion: Parser[PRecursion] = elem('r') ~> elem(':') ~> wholeNumber <~ elem(';') ^^ { num =>
    PRecursion(num.toInt)
  }

  private[this] lazy val pReference: Parser[PReference] = elem('R') ~> elem(':') ~> wholeNumber <~ elem(';') ^^ { num =>
    PReference(num.toInt)
  }
}
