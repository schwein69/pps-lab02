package u02

object Lab2 extends App:

  //Task 1
  println("Hello World")

  //Task 2
  def multi(x: Double, y: Double): Double = x * y

  def curriedMulti(x: Double)(y: Double) = x * y

  def curriedMultiDifference(a: Int)(b: Int)(c: Int): Int = a - b - c

  def multWithTuple(tuple: (Int, Int)): Int = tuple._1 * tuple._2


  def divide(x: Double, y: Double): Double = x / y

  def divideCurried(x: Double)(y: Double): Double = x / y

  def divideCurriedWithTuple(tuple1: (Double, Double))(tuple2: (Double, Double)) = (tuple1._1 / tuple2._1, tuple1._2 / tuple2._2)

  val multiplyBy3: Double => Double = curriedMulti(3)

  println(multi(3, 4))
  println(curriedMulti(3)(4))
  println(curriedMultiDifference(5)(4)(3))
  println(multiplyBy3(3))
  println(divide(5, 2))
  println(divideCurried(5)(2))
  println(multWithTuple((9, 5)))
  println(divideCurriedWithTuple((9, 5))(3, 2))

  //Task 3
  val v = 5
  val positive = v match
    case n if n >= 0 => "positive"
    case _ => "negative"

  def positiveMethod(x: Int): String = x match
    case p if p >= 0 => "positive"
    case _ => "negative"

  println(positiveMethod(5))
  println(positive)


  def negFunction(pred: (String => Boolean)): String => Boolean = s => !pred(s)

  def genericNeg[A](pred: (A => Boolean)): A => Boolean = s => !pred(s)

  val neg: (String => Boolean) => (String => Boolean) = pred => (s => !pred(s))

  val empty: String => Boolean = _ == ""
  val notEmpty = neg(empty) // which type of notEmpty?

  println(notEmpty)
  println(notEmpty("foo")) // true
  println(notEmpty("")) // false
  notEmpty("foo") && !notEmpty("")

  println(negFunction(empty)("foo"))
  println(genericNeg(empty)("foo"))

  //TASK 4

  val p1: (Int, Int, Int) => Boolean = (x, y, z) => x < y && y == z

  val p2: Int => Int => Int => Boolean = x => y => z => x < y && y == z

  /*with currying*/
  def p3(x: Int)(y: Int)(z: Int): Boolean = x < y && y == z

  /*without currying*/
  def p4(x: Int, y: Int, z: Int): Boolean = x < y && y == z

  // Test cases
  val testCases = List(
    (1, 2, 2), // Expected: true
    (2, 3, 3), // Expected: true
    (5, 5, 5), // Expected: false
    (3, 2, 2), // Expected: false
    (0, 1, 2) // Expected: false
  )

  // Running tests
  for ((x, y, z) <- testCases)
    println(s"p1($x, $y, $z) = ${p1(x, y, z)}")
    println(s"p2($x)($y)($z) = ${p2(x)(y)(z)}")
    println(s"p3($x)($y)($z) = ${p3(x)(y)(z)}")
    println(s"p4($x, $y, $z) = ${p4(x, y, z)}")
    println("-" * 30)


  //TASK 5
  def composeFunction(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))

  println(composeFunction(_ - 1, _ * 2)(5))

  def genericComposeFunction[A](f: A => A, g: A => A): A => A = x => f(g(x))

  println(genericComposeFunction[Int](_ - 1, _ * 2)(5))

  //TASK 6
  def composeThree[A, B, C, D](f: C => D, g: B => C, h: A => B): A => D = x => f(g(h(x)))

  println(composeThree[Int, Int, String, String](_ + "!", _.toString, _ * 2)(3))

  //TASK 7
  def power(base: Double, exponent: Int): Double = exponent match
    case a if a > 0 => base * power(base, exponent - 1)
    case 0 => 1
    case c if c < 0 => throw new IllegalStateException()

  def powerTail(base: Double, exponent: Int): Double =
    @annotation.tailrec
    def _power(startExponent: Int, sum: Double): Double = startExponent match
      case a if a > 0 => _power(startExponent - 1, base * sum)
      case 0 => sum
      case c if c < 0 => throw new IllegalStateException()

    _power(exponent, 1)

  println(power(2, 5))
  println(powerTail(2, 5))

  //TASK 8

  def reverseCustom(x: String => Int, y: Int => String): Int => Int = (input: Int) => x(y(input).reverse)

  def reverseCustomShorted(x: Int): Int = x.toString.reverse.toInt

  def reverseCustomShortedNegative(x: Int) = x match
    case a if a < 0 => -(x.toString.tail.reverse).toInt
    case _ => reverseCustomShorted(x)

  val y: Int => String = (n: Int) => n.toString // Convert Int to String
  val x: String => Int = (s: String) => s.toInt // Convert String to Int

  def reverseNumberTail(n: Int): Int =
    @annotation.tailrec
    def _reverse(remaining: Int, acc: Int): Int = remaining match
      case 0 => acc
      case _ => _reverse(remaining / 10, acc * 10 + remaining % 10)

    _reverse(n, 0)

  val num = 12345
  println(s"num: $num, reversed: ${reverseCustom(x, y)(num)}")
  println(reverseCustomShorted(num))
  println(reverseCustomShortedNegative(-123))
  println(reverseNumberTail(num))

  //TASK 9
  enum Expr:
    case Literal(const: Int)
    case Add(exp1: Expr, exp2: Expr)
    case Multiply(exp1: Expr, exp2: Expr)

  def evaluate(expr: Expr): Int = expr match
    case Expr.Literal(const) => const
    case Expr.Add(exp1, exp2) => evaluate(exp1) + evaluate(exp2)
    case Expr.Multiply(exp1, exp2) => evaluate(exp1) * evaluate(exp2)

  def show(expr: Expr): String = expr match
    case Expr.Literal(const) => const.toString
    case Expr.Add(exp1, exp2) => "(" + show(exp1) + " + " + show(exp2) + ")"
    case Expr.Multiply(exp1, exp2) => s"(${show(exp1)} * ${show(exp2)})"
