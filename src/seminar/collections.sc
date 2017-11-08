//Vector(1,2,3) ++ List(3,4,5)
//
//List(true,true,false).map{
//  x => if(x) 1 else 2
//}
//
//List(1,2,3,4).collect{
//  case x if x%2 == 0 => (x+1).toString
//}
//
//List(1,2,3,4).flatMap{
//  x => List.fill(x)(x)
//}
//
//List(1,2,3,4).map{
//  x => List.fill(x)(x)
//}.flatten
//
//for{
//  x<-List(1,2,3,4)
//  y<-List.fill(x)(x)
//}  yield x
//
//List(4,5,6).toList
//
//List((1,"foo"),(2,"bar"),(1,"foo1")).toMap
//
//List().headOption
//
//
//"asdasasas".collectFirst{
//  case c if c > 'd' => c.toInt
//}
//
//List.range(1,16).tail
//List.range(1,16).init
//
//List.range(1,16).withFilter(_ % 2==0)
//
//Vector.range(1,10).groupBy(_ % 3)
//
//List(1,4,7).exists(_ > 10)

List(1,2,3,4).foldLeft(""){
  (s,e) => s"($s,$e)"
}

List(1,2,3,4).foldRight(""){
  (s,e) => s"($s,$e)"
}

Vector.range(1,8)

Vector.range(1,8).aggregate(List[Int]())(
  (list,el) => el::list,
  (l1,l2) => l2:::l1
)

1 to 10 toList
  9 to 1 by(-1) toList
//'a' to 'z' by(2) mkString

