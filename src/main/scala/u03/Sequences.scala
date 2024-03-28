package u03

import u02.AnonymousFunctions.l
import u03.Optionals.Optional
import u03.EncapsulatedSequences.Sequence.nil
import u02.Modules.Person

import scala.annotation.tailrec

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()

    // Lab 03
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
       case (Cons(h,t), Cons(h2, t2) ) => Cons((h, h2), zip(t, t2))
       case _ => Nil()

    def take[A](l: Sequence[A])(n: Int): Sequence[A] = (l, n) match
      case(Cons(h, t), n) if n > 0 => Cons(h, take(t)(n-1))
      case _ => Nil()
    
//    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = (l1, l2) match
//      case (Cons(h,t), l2 ) => Cons(h, concat(t, l2))
//      case (Nil(), l2) => l2
//      case _ => Nil()

    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = l1 match
      case Cons(h,t) => Cons(h, concat(t, l2))
      case Nil() => l2

    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
      case _ => Nil()

    def map2[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] =
      flatMap(l)(v => Cons(mapper(v), Nil()))

    def filter2[A](l: Sequence[A])(pred: A => Boolean): Sequence[A] = flatMap(l)(v => pred(v) match
      case true => Cons(v, Nil()) 
      case _    => Nil()
    )

    def min(l: Sequence[Int]): Optional[Int] = l match
      case Cons(h, Nil()) => Optional.Just(h)
      case Cons(h1, Cons(h2, t)) if h1 <= h2 => min(Cons(h1, t))
      case Cons(h1, Cons(h2, t)) if h2 < h1 => min(Cons(h2, t))
      case _ => Optional.Empty()

    def teacherCourses(l: Sequence[Person]) : Sequence[String] = map(filter(l)(_ match
      case Person.Teacher(_, _) => true
      case _ => false
    ))(_ match
      case Person.Teacher(_, c) => c
    )

    def teacherCourses2(l: Sequence[Person]) : Sequence[String] = flatMap(l)(_ match
      case Person.Teacher(_, c) => Cons(c, Nil())
      case _ => Nil()
    )

    def foldLeft[A, B](l: Sequence[A])(d: B)(f: (B, A) => B) : B = l match
      case Cons(h, t) => foldLeft(t)(f(d, h))(f)
      case _=> d

    extension [A] (s: Sequence[A])
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


//    def sum(l: Sequence[Int]): Int = l match
//      case Cons(h, t) => h + sum(t)
//      case _ => 0
@main def trySequences =
  import Sequences.*
  import  Sequences.Sequence.*
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  val lst = Cons(3, Cons(7, Cons(1, Cons(5 , Nil()) ) ) )


  println(foldLeft(lst)(0)(_ - _))

  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52

  var s1 = Person.Student("mario", 2015)
  var s2 = Person.Student("luca", 2016)
  var t1 = Person.Teacher("davide", "ml")
  var t2 = Person.Teacher("mirko", "sd")
  var t3 = Person.Teacher("mirko", "lcmc")
  var t4 = Person.Teacher("mirko", "pps")

  var tc = Cons(s1, Cons(t1, Cons(s2, Cons(t2, Cons(t3, Cons(t3, Nil()))))))

  println("lista")
  println(teacherCourses2(tc))

  var l1 = Cons("ml", Nil())
  var l2 = Cons("sd", Nil())

  var l1and2 = concat(l1, l2)

  println(l1and2)


