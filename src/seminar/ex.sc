import scala.util.Try

def safeDivide(x: Int, y: Int): Option[Int] =
  try {
  Some(x/y)
}
catch {
  case ex: ArithmeticException => None
}

def safeDiv(x: Int, y: Int): Try[Int] = Try(x/y)

safeDiv(7,0)


abstract class AbstractTest {
  println("1")
  def name: String = "start"
}
trait NameProvider extends AbstractTest{
  override def name: String = "<foo>" + super.name + "</foo>"
}
trait SomeMarker extends AbstractTest{
  override def name: String = "<bar>" + super.name + "</bar>"
}

class ConcreteClass extends AbstractTest with NameProvider with SomeMarker {
}

val e = new ConcreteClass

e.name

new AbstractTest {
  println("2")
  override def name = "an"
}.name


abstract class AbstractTest1 {
  val name: String
}

trait Req{
  self: AbstractTest =>

  val x = new Object{
    self.name = ""
  }
}

object x extends AbstractTest1 with Req{
  override val name = "x"
}

//val r =  new  {
//  override def name = {
//    println("2")
//    "an"}
//} with AbstractTest
//r.name

