package book.scala_with_cats

object Ch1 extends App {
  // Define a very simple JSON AST
  sealed trait Json
  final case class JsObject(get: Map[String, Json]) extends Json
  final case class JsString(get: String) extends Json
  final case class JsNumber(get: Double) extends Json
  final case object JsNull extends Json

  // The "serialize to JSON" behaviour is encoded in this trait
  trait JsonWriter[A] {
    def write(value: A): Json
  }

  final case class Person(name: String, email: String)

  object JsonWriterInstances {
    implicit val stringWriter: JsonWriter[String] =
      (value: String) => JsString(value)
    implicit val personWriter: JsonWriter[Person] =
      (value: Person) =>
        JsObject(
          Map(
            "name" -> JsString(value.name),
            "email" -> JsString(value.email)
          )
        )
  }

  // Interface object
  object Json {
    def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }

  // Interface syntax
  object JsonSyntax {
    implicit class JsonWriterOps[A](value: A) {
      def toJson(implicit w: JsonWriter[A]): Json =
        w.write(value)
    }
  }

  import JsonWriterInstances._
  import JsonSyntax._

  // Interface object
  Json.toJson(Person("Dave", "dave@example.com"))

  // Interface syntax
  Person("Dave", "dave@example.com").toJson

  //  implicitly Method
  implicitly[JsonWriter[String]]

  implicit def optionWriter[A](implicit
      writer: JsonWriter[A]
  ): JsonWriter[Option[A]] = {
    case Some(aValue) => writer.write(aValue)
    case None         => JsNull
  }

  Json.toJson(Option("A string"))

  trait Printable[A] {
    def format(value: A): String
  }

  object Printable {
    def format[A](value: A)(implicit printer: Printable[A]): String =
      printer.format(value)

    def print[A](value: A)(implicit printer: Printable[A]): Unit =
      println(printer.format(value))
  }

  object PrintableInstances {
    implicit val intPrintable: Printable[Int] =
      String.valueOf(_)

    implicit val stringPrintable: Printable[Int] =
      _.toString
  }

  import PrintableInstances._

}
