package com.thiago.typeclassopedia

sealed trait Maybe[A]

case class Just[A](a: A) extends Maybe[A]
case class Empty[A]() extends Maybe[A]
