package foo
package bar

//import java.lang.String

//class C
//
//class D(x: Int) extends C

case class CC1(i: Int, d: Double, s: String) {
  me =>

  def compute = s.length + i + d

  def echo(a: Any*) = a

  def render: String = f"CC@${System.identityHashCode(me)}%08x: i=$i; d=$d; s='$s'"
}



trait CakeMe {
  def cakeMe: CakeMe

  trait CakeMe {
    def variety: String
    def quantity: Double
  }
}


trait   ChocCake
extends CakeMe
{
  def cakeMe = new ChocCake

  class   ChocCake
  extends CakeMe
  {
    def variety = "Chocolate"
    def quantity = 500.0
  }
}


class C1(a1: Any, a2: Any) {
  (a1, a2) match {
    case (i1: Int, i2: Int)     => println(s"i1=$i1; i2=$i2")
    case (i: Int,  _)           => println(s"i1=$i")
    case (_,       i: Int)      => println(s"i2=$i")
    case (x,       y) if x != y => println(s"x=$x; y=$y")
    case (x,       y)           => println(s"x/y=$y")
  }


  def seqStuff(s1: Seq[Any], s2: Seq[Any], s3: Seq[Any]): Unit = {
    for {
      ss1 <- s1
      ss2 <- s2
      ss3 <- s3
    }
    yield
      s"$ss1:$ss2:$ss3"
  }
}


trait T {
  def t: String
}

trait   T1
extends T
{
  def t1: String = "T1"
  override def t = t1
}

trait   T2
extends T
{
  def t2: String = "T2"
  override def t = t2
}

trait   T3
extends T
{
  def t3: String = "T3"
  override def t = t3
}

class   FourT(val count: Int)
extends T3
with    T2
with    T1
{
  def identify: String = s"id=$t; count=$count"
}


object Simple {
  def main(args: Array[String]): Unit = {
    val fourT1 = new FourT(23)
    println(s"fourT1=$fourT1; fourT1.identify=${fourT1.identify}")
  }

  val sym1 = 'glorch

  val `!@#$%^&*()` = 42

  println("Simple constructor")

  var i = 100

  while (i > 0) i -= 1
}
