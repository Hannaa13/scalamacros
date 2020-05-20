import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object ImplicitMacros extends App {

  trait Showable[T] {
    def show(x: T): String
  }

  def show[T](x: T)(implicit s: Showable[T]) = s.show(x)

  //    implicit object IntShowable extends Showable[Int] {
  //      def show(x: Int): String = x.toString
  //    }
  //  implicit val strShowable: Showable[String] = identity

  object Showable {
    implicit def materializeShowable[T]: Showable[T] = macro showableImpl[T]

    def showableImpl[T: c.WeakTypeTag](c: blackbox.Context): c.universe.Tree = {
      val t = c.weakTypeOf[T]
      q"""
       new Showable[$t] {
         def show(x: $t) = x.toString
       }
     """
    }
  }

}
