package other

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object Macro extends App {
  class ToStringObfuscate(fieldsToObfuscate: String*) extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro ToStringObfuscateImpl.impl
  }

  object ToStringObfuscateImpl {
    def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = ???
  }
}
