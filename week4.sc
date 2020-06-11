abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

class Succ(n: Nat) extends Nat {
  def isZero = false

  def predecessor: Nat = n

  def successor: Nat = new Succ(this)

  def +(that: Nat): Nat = new Succ(n + that)

  def -(that: Nat): Nat =
    if (that.isZero) this
    else this - that
}

object Zero extends Nat {
  def isZero = true

  def predecessor = throw new Error("0.predecessor")

  def successor: Nat = new Succ(this)

  def +(that: Nat): Nat = that

  def -(that: Nat) =
    if (that.isZero) this
    else throw new Error("0 can't subtract; natural numbers are >= 0")
}


object exercises {
  val zero = Zero
  val one = new Succ(zero)
  val two = new Succ(one)
  val three = new Succ(two)
  val four = new Succ(three)
  val five = new Succ(four)
  val six = new Succ(five)
  val seven = new Succ(six)
  val eight = new Succ(seven)

  println(eight - five)
}


object List {
  def apply[T]() = new Nil
  def apply[T](x: T): List[T] = new Cons(x1, new Nil)
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
}

// the data was added piecemeal
val (source, ship_method, sort_code, external_sort_code, end_time, transit_time) = leg.split("\\|") match {
  case Array(source, ship_method, sort_code, external_sort_code, end_time, transit_time) => (source, ship_method, sort_code, external_sort_code, end_time, transit_time)
  case Array(source, ship_method, sort_code, external_sort_code, end_time) => (source, ship_method, sort_code, external_sort_code, end_time, "-1")
  case Array(source, ship_method, external_sort_code, end_time) => (source, ship_method, "", external_sort_code, end_time, "-1")
}

trait Expr
case class Number(n: Int) extends Expr
case class Var(n: String) extends Expr
case class Product(x: Expr, y: Expr) extends Expr
case class Sum(x: Expr, y: Expr) extends Expr

//def show(e: Expr): String = {
//  def handleSumsNeedingBrackets(expr: Expr): String = ex match {
//    case Sum(_, _) => "(" + show(ex) + ")"
//    case _ => show(ex)
//  }
//
//  e match {
//    case Number(x) => x.toString
//    case Var(x) => x
//    case Sum(l, r) => show(l) + " + " + show(r)
//    case Product(l, r) => handleSumsNeedingBrackets(l) + " * " + handleSumsNeedingBrackets(r)
//  }
//}

def show(e: Expr): String = e match {
  case Number(n) => n.toString
  case Var(v) => v
  case Sum(l, r) => show(l) + " + " + show(r)
  case Product(l: Sum, r: Sum) => "(" + show(l) + ") * (" + show(r) + ")"
  case Product(l: Sum, r) => "(" + show(l) + ") * " + show(r)
  case Product(l, r: Sum) => show(l) + " * (" + show(r) + ")"
  case Product(l, r) => show(l) + " * " + show(r)
}

show(Product(Sum(Number(1),Number(2)),Sum(Number(3),Number(4))))