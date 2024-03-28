package consegna

 // Task 1

 // a)

def take[A](l: Sequence[A])(n: Int): Sequence[A] = (l, n) match
 case (Cons(h, t), n) if n > 0 => Cons(h, take(t)(n - 1))
 case _ => Nil()

// b)

def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
  case (Cons(h, t), Cons(h2, t2)) => Cons((h, h2), zip(t, t2))
  case _ => Nil()

// c)

def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = l1 match
  case Cons(h, t) => Cons(h, concat(t, l2))
  case Nil() => l2

// d)

def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
  case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
  case _ => Nil()

// e)

def map2[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] =
  flatMap(l)(v => Cons(mapper(v), Nil()))

def filter2[A](l: Sequence[A])(pred: A => Boolean): Sequence[A] = flatMap(l)(v => pred(v) match
  case true => Cons(v, Nil())
  case _ => Nil()
)

// Task 2

def min(l: Sequence[Int]): Optional[Int] = l match
  case Cons(h, Nil()) => Optional.Just(h)
  case Cons(h1, Cons(h2, t)) if h1 <= h2 => min(Cons(h1, t))
  case Cons(h1, Cons(h2, t)) if h2 < h1 => min(Cons(h2, t))
  case _ => Optional.Empty()

// Task 3

def teacherCourses(l: Sequence[Person]): Sequence[String] = map(filter(l)(_ match
  case Person.Teacher(_, _) => true
  case _ => false
))(_ match
  case Person.Teacher(_, c) => c
)

def teacherCourses2(l: Sequence[Person]): Sequence[String] = flatMap(l)(_ match
  case Person.Teacher(_, c) => Cons(c, Nil())
  case _ => Nil()
)

// Task 4

def foldLeft[A, B](l: Sequence[A])(d: B)(f: (B, A) => B): B = l match
  case Cons(h, t) => foldLeft(t)(f(d, h))(f)
  case _ => d

// Task 5

extension[A] (s: Sequence[A])
  def zip2[B](l: Sequence[B]): Sequence[(A, B)] = (s, l) match
    case (Cons(h, t), Cons(h2, t2)) => Cons((h, h2), t.zip2(t2))
    case _ => Nil()
  def take2(n: Int): Sequence[A] = (s, n) match
    case (Cons(h, t), n) if n > 0 => Cons(h, t.take2(n - 1))
    case _ => Nil()
  def concat2(l: Sequence[A]): Sequence[A] = s match
    case Cons(h, t) => Cons(h, s.concat2(l))
    case Nil() => l
  def flatMap2[B](mapper: A => Sequence[B]): Sequence[B] = s match
    case Cons(h, t) => mapper(h).concat2(t.flatMap2(mapper))
    case _ => Nil()
  def map3[B](mapper: A => B): Sequence[B] =
    s.flatMap2(v => Cons(mapper(v), Nil()))
  def filter3(pred: A => Boolean): Sequence[A] = s.flatMap2(v => pred(v) match
    case true => Cons(v, Nil())
    case _ => Nil()
  )
  def foldLeft2[B](d: B)(f: (B, A) => B): B = s match
    case Cons(h, t) => t.foldLeft2(f(d, h))(f)
    case _ => d

extension (s: Sequence[Int])
  def min2(): Optional[Int] = s match
    case Cons(h, Nil()) => Optional.Just(h)
    case Cons(h1, Cons(h2, t)) if h1 <= h2 => Cons(h1, t).min2()
    case Cons(h1, Cons(h2, t)) if h2 < h1 => Cons(h2, t).min2()
    case _ => Optional.Empty()

// Task 7

def fill[A](n: Int)(k: A): Stream[A] = n match
  case n if n > 0 => cons(k, fill(n - 1)(k))
  case _ => Empty()

// Task 8

val pell: Stream[Int] =
  def buildPellSequence(p1: Int)(p2: Int): Stream[Int] =
    val v = 2 * p1 + p2
    cons(v, buildPellSequence(v)(p1))

  cons(0, cons(1, buildPellSequence(1)(0)))
