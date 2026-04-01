package it.unibo.pps.tasks.typeclasses

import it.unibo.pps.u03.Sequences.Sequence, Sequence.*
import it.unibo.pps.u03.extensionmethods.Optionals.Optional, Optional.*

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  def log[A](a: A): Unit = println("The next element is: "+a)

  trait Traversable[T[_]]:
    extension [A](container: T[A])
      def foreach(f: A => Unit): Unit

  given Traversable[Sequence] with
    extension [A](container: Sequence[A])
      def foreach(f: A => Unit): Unit = container match
        case Cons(h, t) =>
          f(h)
          t.foreach(f)
        case Nil() => ()

  given Traversable[Optional] with
    extension [A](container: Optional[A])
      def foreach(f: A => Unit): Unit = container match
        case Just(a) => f(a)
        case None() => ()

  def logAll[T[_] : Traversable, A](container: T[A]): Unit =
    container.foreach(log)


@main def testTraversable(): Unit =
  import Ex5Traversable.*

  val seq = Cons(10, Cons(20, Nil()))
  val opt = Optional.Just("Hello")
  val emptyOpt = Optional.None[Int]()

  println("--- Logging Sequence ---")
  logAll(seq)

  println("\n--- Logging Optional ---")
  logAll(opt)
    

  
