package u03

import org.junit.*
import org.junit.Assert.*

import u03.Task.* 
import u03.Streams.*
import Stream.*
import u03.Sequences.*
import Sequence.*
import u02.Modules.Person
import u03.Optionals.*
import u03.Optionals.Optional.*


class TaskTest:

  @Test def testTake(): Unit =
    val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))
    assertEquals(Cons(10, Cons(20, Nil())), l.take(2))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), l.take(3))
    assertEquals(Nil(), l.take(0))
    assertEquals(Nil(), Nil().take(2))
  
  @Test def testZip(): Unit =
    val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), l.zip(l2))
    assertEquals(Nil(), l.zip(Nil()))
    assertEquals(Nil(), Nil().zip(l2))
    assertEquals(Nil(), Nil()zip(Nil()))

  @Test def testConcat(): Unit =
    val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), l.concat(l2))
    assertEquals(Cons(40, Cons(50, Nil())), Nil().concat(l2))

  @Test def testFlatMap(): Unit =
    val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), l.flatMap(v => Cons(v + 1, Nil())))
    assertEquals(Nil(), Nil().flatMap(v => Cons(v, Nil())))

  @Test def testMap(): Unit =
    val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), l.map(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), l.map(_ + ""))

  @Test def testFilter(): Unit =
    val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))
    assertEquals(Cons(20, Cons(30, Nil())), l.filter(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), l.filter(_ != 20))
  
  @Test def testMin(): Unit =
    val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))
    assertEquals(Just(10), Task.min(l))
    assertEquals(Just(1), Task.min(Cons(1, Nil())))
    assertEquals(Empty(), Task.min(Nil()))

  @Test def testGetCorses(): Unit =
    val l3: Sequence[Person] = Cons(Person.Teacher("Viroli", "PPS"), Cons(Person.Teacher("Ghini", "SO"), Nil()))
    assertEquals(Cons("PPS", Cons("SO", Nil())), courses(l3))

  @Test def testFoldLeft(): Unit =
    val lst = Cons(3,Cons(7,Cons(1,Cons(5, Nil()))))
    assertEquals(-16, foldLeft(lst)(0)(_- _))

  @Test def testTakeWhile(): Unit = 
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.takeWhile(str1)(_ < 5) // {0,1,2,3,4}
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))), Stream.toList(str2))

  @Test def testFill(): Unit =
    assertEquals(Cons("a", Cons("a", Cons("a", Nil()))), Stream.toList(Stream.fill(3)("a")))

  @Test def testPell(): Unit =
    val l = Cons(0, Cons(1, Cons(2, Cons(5, Cons(12, Nil())))))
    assertEquals(Cons(0, Nil()), Stream.toList(Stream.take(pell)(1)))
    assertEquals(l, Stream.toList(Stream.take(pell)(5)))