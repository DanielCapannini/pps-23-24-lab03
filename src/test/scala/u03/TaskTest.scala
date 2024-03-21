package u03

import org.junit.*
import org.junit.Assert.*

import u03.Task.* 
import u03.Streams.*
import Stream.*
import u03.Sequences.*
import Sequence.*
import u02.Modules.Person
import u02.AlgebraicDataTypes.Person

class TaskTest:

  @Test def testTake() =
    val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))
    assertEquals(Cons(10, Cons(20, Nil())), take(l)(2))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), take(l)(3))
    assertEquals(Nil(), take(l)(0))
    assertEquals(Nil(), take(Nil())(2))
  
  @Test def testZip() = 
    val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), zip(l, l2))
    assertEquals(Nil(), zip(l, Nil()))
    assertEquals(Nil(), zip(Nil(), l2))
    assertEquals(Nil(), zip(Nil(), Nil()))

  @Test def testConcat() =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), concat(l, l2))
    assertEquals(Cons(40, Cons(50, Nil())), concat(Nil(), l2))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))
  
  @Test def testMin() =
    val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))
    assertEquals(Just(10), min(l))
    assertEquals(Just(1), min(Cons(1, Nil())))
    assertEquals(Empty(), min(Nil()))

  @Test def testGetCorses() =
    val l3: Sequence[Person] = Cons(Person.Teacher("Viroli", "PPS"), Cons(Person.Teacher("Ghini", "SO"), Nil()))
    assertEquals(Cons("PPS", Cons("SO", Nil())), getCourses(l3))

  @Test def testFoldLeft() =
    val lst = Cons(3,Cons(7,Cons(1,Cons(5, Nil()))))
    assertEquals(-16, foldLeft(lst)(0)(_- _))

  @Test def testTakeWhile(): Unit = 
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.takeWhile(str1)(_ < 5) // {0,1,2,3,4}
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))), Stream.toList(str2))

  @Test def testFill() =
    assertEquals(Cons(a, Cons(a, Cons(a, Nil()))), Stream.toList(Stream.fill(3)("a")))
    