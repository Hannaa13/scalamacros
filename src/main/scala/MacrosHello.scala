import scala.reflect.macros.blackbox

// Macro bundles
object MacrosHelloWorld {

  def hello(): Unit = macro Impl.helloImpl

  // макрос с параметром
  def hello2(str: String): Unit = macro Impl.hello2Impl

  def isEvenLog(n: Int): Unit = macro Impl.isEvenLogImpl
}

class Impl(val c: blackbox.Context) {

  def isEvenLogImpl(n: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._
    c.Expr {
      q"""
        if ($n%2==0) {
        println($n.toString + " is even")
        } else println($n.toString + " is odd")
        """
    }
  }

  def helloImpl(): c.Expr[Unit] = {
    import c.universe._
    c.Expr(q"""println("hello!")""")
  }

  // другой способ
  // reify - (из пакета universe) конвертирует выражение в c.Expr[]
  def helloImpl2(): c.Expr[Unit] = {
    import c.universe._
    reify {
      println("hello!")
    }
  }

  def hello2Impl(str: c.Expr[String]): c.Expr[Unit] = {
    import c.universe._
    c.Expr(q"""println("hello " + ${str.tree} + "!")""")
  }

  def hello2Impl2(s: c.Expr[String]): c.Expr[Unit] = {
    import c.universe._
    reify {
      println(s"hello ${s.splice}")
    }
  }

}

object MacrosAdd {
  def add(n1: Int, n2: Int): Int = macro Impl2.addImpl
}

class Impl2(val c: blackbox.Context) {
  def addImpl(n1: c.Expr[Int], n2: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._
    reify {
      n1.splice + n2.splice
    }
  }
}