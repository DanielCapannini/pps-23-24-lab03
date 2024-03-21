package u03

import Sequences.* 
import Sequence.*
import Optionals.*
import Streams.*
import u02.AlgebraicDataTypes.IntList
import u02.Modules.isStudent
import u02.Modules.Person
import u02.Modules

object Task extends App:

    //part 1

    //1
    def take[A](l: Sequence[A], n: Int): Sequence[A] = (l, n) match
        case (_, 0) | (Nil(), _) => Nil()
        case (Cons(head, tail), _) => Cons(head, take(tail, n - 1))

    def zip[A,B](l: Sequence[A], r: Sequence[B]): Sequence[(A,B)] = (l, r) match
        case (_, Nil()) | (Nil(), _) => Nil()
        case (Cons(head1, tail1), Cons(head2, tail2)) => Cons((head1, head2), zip(tail1, tail2))

    def concat[A](l: Sequence[A], r: Sequence[A]): Sequence[A] = (l, r) match
        case (Nil(), _) => r
        case (Cons(head, tail), _) => Cons( head, concat(tail, r))

    def flatMap[A,B](l: Sequence[A])(f: A=> Sequence[B]): Sequence[B] = l  match
        case (Nil()) => Nil()
        case (Cons(head, tail)) => concat(f(head), flatMap(tail)(f))

    def map[A, B](l: Sequence[A])(f: A => B): Sequence[B] = flatMap(l)(a => Cons(f(a), Nil()))

    def filter[A](l: Sequence[A])(pred: A => Boolean): Sequence[A] = flatMap(l)(a => if (pred(a)) Cons(a, Nil()) else Nil())

    //2
    def min(l: Sequence[Int]): Optional[Int] = l match
        case (Cons(head, tail)) if min(filter(tail)(_<head)) != Optional.Empty() => min(filter(tail)(_<head))
        case (Cons(head, tail)) if min(filter(tail)(_<head)) == Optional.Empty() => Optional.Just(head)
        case (Nil()) => Optional.Empty()
        case (Cons(_, _)) => Optional.Empty()

    //part 2

    //3
    def getCourses(s: Sequence[Modules.Person]): Sequence[String] = 
        flatMap(s):
            case Modules.Person.Teacher(_, c) => Cons(c, Nil())
            case _ => Nil()
    
    //4
    def foldLeft(s: Sequence[Int])(x: Int)(f: (Int, Int) => Int): Int = s match
        case (Nil()) => x
        case (Cons(head, tail)) => foldLeft(tail)(f(x, head))(f)

    //part 3

    import Stream.*

    //6
    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
      case _ => Empty()

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
