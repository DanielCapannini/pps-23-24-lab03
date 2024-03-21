package u03

import Sequences.* 
import Sequence.*
import Optionals.*
import Streams.*
import Stream.*
import u02.AlgebraicDataTypes.IntList
import u02.Modules.isStudent
import u02.Modules.Person
import u02.Modules

object Task:
    //part 1
  extension [A](l: Sequence[A])
    //1
    def take(n: Int): Sequence[A] = (l, n) match
      case (_, 0) | (Nil(), _) => Nil()        
      case (Cons(head, tail), _) => Cons(head, tail.take(n - 1))

    def zip[B](r: Sequence[B]): Sequence[(A,B)] = (l, r) match
      case (_, Nil()) | (Nil(), _) => Nil()
      case (Cons(head1, tail1), Cons(head2, tail2)) => Cons((head1, head2), tail1.zip(tail2))

    def concat(r: Sequence[A]): Sequence[A] = (l, r) match
      case (Nil(), _) => r
      case (Cons(head, tail), _) => Cons( head, tail.concat(r))

    def flatMap[B](f: A=> Sequence[B]): Sequence[B] = l  match
      case (Nil()) => Nil()
      case (Cons(head, tail)) => f(head).concat(tail.flatMap(f))

    def map[B](f: A => B): Sequence[B] = l.flatMap(a => Cons(f(a), Nil()))

    def filter(pred: A => Boolean): Sequence[A] = l.flatMap(a => if (pred(a)) Cons(a, Nil()) else Nil())
  

    //2
  def min(l: Sequence[Int]): Optional[Int] = l match
    case Cons(head1, Cons(head2, tail)) if head1 > head2 => min(Cons(head2, tail))
    case Cons(head1, Cons(head2, tail)) if head1 <= head2 => min(Cons(head1, tail))
    case Cons(head, Nil()) => Optional.Just(head)
    case Nil() => Optional.Empty()
    

    //part 2

    //3
  def courses(s: Sequence[Modules.Person]): Sequence[String] = 
    s.flatMap:
      case Modules.Person.Teacher(_, c) => Cons(c, Nil())
      case _ => Nil()
    
    //4
  def foldLeft(s: Sequence[Int])(x: Int)(f: (Int, Int) => Int): Int = s match
    case (Nil()) => x
    case (Cons(head, tail)) => foldLeft(tail)(f(x, head))(f)

    //part 3
 
    //6 
    /*
    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail)(pred))
      case _ => empty()
    */

    //7
  def fill[A](n: Int)(k: A): Stream[A] = n match
    case n if n>0 => cons(k, fill(n-1)(k))
    case _ => empty()

    //8
  val pell: Stream[Int] = {
    def pellHelper(a: Int, b: Int): Stream[Int] =
      cons(a, pellHelper(b, 2 * b + a))
    pellHelper(0, 1)
  }
