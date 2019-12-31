package com.danielasfregola.tutorial.cat.applicative

import com.danielasfregola.tutorial.cat._

// See solution at https://gist.github.com/DanielaSfregola/ddf48f6c5638f6284b563798c55d5ebd

object ApplicativeInstances {

  implicit val maybeApplicative: Applicative[Maybe] = new Applicative[Maybe] {
    override def pure[A](a: A): Maybe[A] = Just(a)

    override def ap[A, B](boxF: Maybe[A => B])(boxA: Maybe[A]): Maybe[B] =
      (boxA,boxF) match {
      case (Empty,_) => Empty
      case (_,Empty) => Empty
      case (Just(a),Just(f)) =>  Just(f(a))
    }
  }

  implicit val zeroOrMoreApplicative: Applicative[ZeroOrMore] = new Applicative[ZeroOrMore] {
    override def pure[A](a: A): ZeroOrMore[A] = OneOrMore(a, Zero)

    override def ap[A, B](boxF: ZeroOrMore[A => B])(boxA: ZeroOrMore[A]): ZeroOrMore[B] = (boxA, boxF) match {
      case (Zero, _) => Zero
      case (_, Zero) => Zero
      case (OneOrMore(a, Zero), OneOrMore(f, fs)) => OneOrMore(f(a), ap(fs)(boxA))
      case (OneOrMore(a,as), OneOrMore(f,Zero)) => OneOrMore(f(a),Zero)
      case (OneOrMore(a,as), OneOrMore(f,fs)) =>{

      }

    }
    }
  }