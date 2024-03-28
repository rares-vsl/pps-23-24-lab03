package u02

import u03.EncapsulatedSequences.Sequence.nil
import u03.Sequences.*
import Sequence.*

object Modules extends App :

  // An ADT: type + module
  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:
    def name(p: Person): String = p match
      case Student(n, _) => n
      case Teacher(n, _) => n
    def courses(l: Sequence[Person]): Sequence[String] = l match
        case Cons(Teacher(n, c), t) => Cons(c, courses(t))
        case Cons(Student(_, _), t) => courses(t)
        case Nil()          => Nil()
      // def _inList(l: Sequence[String]): Boolean = l match
      //   case Cons(h1, Nil()) => true
      //   case Cons(h1, Cons(h2, _))  if h1 == h2 => false
      //   case Cons(h1, Cons(h2, t)) if h1 != h2 => _inList(Cons(h2, t))
      //   case Cons(h1, Cons(h2, Nil())) if h1 != h2 => true

      // val  all = _allCourser(l)
      // filter(all)(_ == true)
        

    // def coursesUnique (l: Sequence[Teacher]) : Sequence[String] =
    //   filter(courses(l))(true)

    def mapToCourses(s: Sequence[Person]): Sequence[String] = 
        flatMap(s)(_ match
            case Teacher(name, course) => Cons(course, Nil())
            case _ => Nil()
        )


  println(Person.name(Person.Student("mario", 2015)))

  var s1 = Person.Student("mario", 2015)
  var s2 = Person.Student("luca", 2016)
  var t1 = Person.Teacher("davide", "ml")
  var t2 = Person.Teacher("mirko", "ml")

  var tc = Cons(s1, Cons(t1, Cons(s2, Cons(t2, Nil()))))

  println("lista")
  println(Person.courses(tc))

  
  println("lista2")
  println(Person.mapToCourses(tc))

  import Person.*

  println(name(Student("mario", 2015)))

  // a method outside the Person module
  def isStudent(p: Person): Boolean = p match
    case Student(_, _) => true
    case _ => false

  println(isStudent(Student("mario", 2015)))