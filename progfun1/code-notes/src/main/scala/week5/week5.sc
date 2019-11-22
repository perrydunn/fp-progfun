// last of List (Cons List) is O(n)
def last[T](xs: List[T]): T = xs match {
  case List() => throw new Error("last of empty list")
  case List(x) => x
  case _ :: ys => last(ys)
}

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(_) => Nil
  case y :: ys => y :: init(ys)
}

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => xs
  case y :: ys => reverse(ys) ++ List(y)
}

// Or, using List methods: (xs take n) ::: (xs drop n + 1)
def removeAt[T](n: Int, xs: List[T]): List[T] =
  if (n < 0 || xs.isEmpty) xs
  else if (n == 0) xs.tail
  else xs.head :: removeAt(n - 1, xs.tail)

def flatten(xs: List[Any]): List[Any] = {
  if (xs.isEmpty) xs
  else xs.head match {
    case List() => flatten(xs.tail)
    case z :: zs => flatten(List(z)) ::: flatten(zs) ::: flatten(xs.tail)
    case _ => xs.head :: flatten(xs.tail)
  }
}

last(List(1, 2, 3))
init(List(1, 2, 3))
concat(List(1, 2, 3), List(4, 5, 6))
reverse(List(1, 2, 3))
removeAt(1, List('a', 'b', 'c', 'd'))
removeAt(-1, List('a', 'b', 'c', 'd'))
removeAt(5, List('a', 'b', 'c', 'd'))
flatten(List(List(1, 1), 2, List(3, List(5, 8))))


import math.Ordering

def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length
  if (n <= 1) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
    val (fst, snd) = xs splitAt n / 2
    merge(msort(fst), msort(snd))
  }
}

msort(List(1, 5, 3, 8, 0, 2))
msort(List("bananas", "oranges", "apples"))


def squareList(xs: List[Int]): List[Int] =
  xs map (x => x * x)

squareList(List(1, 2, 3, 4))

def pack[T](xs: List[T]): List[List[T]] =
  xs match {
    case Nil => Nil
    case y :: _ =>
      val (fst, snd) = xs span (x => x == y)
      fst :: pack(snd)
  }

def encode[T](xs: List[T]): List[(T, Int)] =
  pack(xs) map (x => (x.head, x.length))

pack(List("a", "a", "a", "b", "c", "c", "a"))
pack(List())
pack(List("a"))
encode(List("a", "a", "a", "b", "c", "c", "a"))
encode(List())
encode(List("a"))


def sum(xs: List[Int]): Int = (0 :: xs) reduceLeft ((x, y) => x + y)
// Or, more succinctly...
def product(xs: List[Int]): Int = (xs foldLeft 1) (_ * _)

sum(List(1, 2, 3, 4))
sum(List())
product(List(1, 2, 3, 4))
product(List())

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())((e, ls) => f(e) :: ls)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((_, acc) => 1 + acc)

mapFun(List(1, 2, 3), (x: Int) => x * x)
mapFun(List(), (x: Int) => x * x)
lengthFun(List(1, 2, 3))
lengthFun(List())
