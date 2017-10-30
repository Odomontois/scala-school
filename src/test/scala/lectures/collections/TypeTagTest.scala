package lectures.collections

import scala.reflect.runtime.universe._

object TypeTagTest extends App {

  def intValue[T: TypeTag](t: T) = intValueInner(t, typeOf[T])

  def intValueInner(value: Any, tpe: Type):Int = tpe match {

    // integer
  case intType if intType <:< typeOf[Int] =>
  value.asInstanceOf[Int]
    // string
  case stringType if stringType <:< typeOf[String] =>
  value.asInstanceOf[String].toInt
    // option of either string or integer
  case optionType @ TypeRef(_, _, typeArg::Nil) if optionType <:< typeOf[Option[Any]] =>
      value.asInstanceOf[Option[_]].map(v => intValueInner(v, typeArg)).getOrElse(0)
  }
  /*
  def intValueInner1[T](value: T)(implicit tag: TypeTag[T]): Int = {
    tag.tpe match {
      // integer
      case intType if intType <:< typeOf[Int] =>
        value.asInstanceOf[Int]
      // string
      case stringType if stringType <:< typeOf[String] =>
        value.asInstanceOf[String].toInt
      // option of either string or integer
      case optionType @ TypeRef(_, _, typeArg::Nil) if optionType <:< typeOf[Option[_]] =>
        println(s"Unwrapped type is $typeArg")
        val option = value.asInstanceOf[Option[_]]
        option.map { optionValue =>
          // how to pass the typeArg here?
          intValue(optionValue)
        }.getOrElse(0)
    }
  }
*/
  println(intValue(1))
  println(intValue("1"))
  println(intValue(Some(Some("1"))))

}
