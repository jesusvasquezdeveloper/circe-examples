import java.awt.datatransfer.FlavorListener
import java.math.MathContext

import io.circe.Decoder.Result
import io.circe._
import io.circe.parser._
import io.circe.generic.JsonCodec
import io.circe.syntax._

import scala.Boolean
import scala.util._

object CirceTester {

  def main(args: Array[String]): Unit = {

    val json =
      """{
  "foo": "bar",
  "baz": "123.34",
  "list": [ "hello", "bye", "whatever" ],
  "jsonlist": [ {"name":"Jesus","age":20,"numbers":[],"dummy":0},{"name":"Pedro","age":45},{"name":"Christian"}]
}
"""


    case class Person(name: String, age: Option[Int], numbers: Option[List[Long]])
    case class IncompleteJson(foo: String, baz: String)

    implicit val decodePerson: Decoder[Person] = {

      Decoder.forProduct3("name", "age", "numbers")(Person.apply)

    }


    implicit val decodeIncompleteJson: Decoder[IncompleteJson] = {

      Decoder.forProduct2("foo", "baz")(IncompleteJson.apply)
    }

    case class MyJson(foo: String, baz: String, list: List[String], jsonlist: List[Person])

    implicit val decodeMyJson: Decoder[MyJson] = {

      Decoder.forProduct4("foo", "baz", "list", "jsonlist")(MyJson.apply)

    }

    class Thing(val foo: String, val bar: Int, val list: List[Int])

    implicit val encodeFoo: Encoder[Thing] = new Encoder[Thing] {
      final def apply(a: Thing): Json = Json.obj(
        ("foo", Json.fromString(a.foo)),
        ("bar", Json.fromInt(a.bar))
      )
    }

    val test = Json.obj(
      ("foo", Json.fromBoolean(true)),
      ("baz", Json.fromString("hello"))

    )

    println(test.hcursor.downField("baz").as[Boolean])

    val printableJson = encodeFoo.apply(new Thing("foo", 1, List(1, 23, 4)))


    println(printableJson.toString().replace("\n", "").replace(", ", ",\n"))


    val decoder: Decoder[Thing] = new Decoder[Thing] {
      override def apply(c: HCursor): Result[Thing] = for {
        foo <- c.downField("foo").as[String]
        bar <- c.downField("bar").as[Int]
        list <- c.downField("list").as[List[Int]]

      } yield new Thing(foo, bar, list)
    }

    val thing: Decoder.Result[Thing] = decoder.decodeJson(printableJson)


    implicit val personEncoder: Encoder[Person] = new Encoder[Person] {

      final def apply(p: Person): Json = {

        Json.obj(
          ("age", Json.fromInt(p.age.get)),
          ("name", Json.fromString(p.name)),
          ("numbers", Json.fromValues(p.numbers.get.map(x => Json.fromLong(x))))


        )

      }
    }


    val persona = Person("Jesus",Some(32),Some(List(123456,78910)))

    val datosPersona = personEncoder.apply(persona)

   datosPersona.hcursor.downField("age").as[BigDecimal] match {
     case Right(success) => println(success)
     case Left(failure) => println(failure)


   }

    val mc = new MathContext(128)

    val c : BigDecimal =  BigDecimal(1.0904034304304903493043943434343343,mc)

    println(c)

    println(datosPersona)


  }

}
