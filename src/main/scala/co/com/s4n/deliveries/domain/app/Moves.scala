package co.com.s4n.deliveries.domain.app

sealed trait Moves
  case class A() extends Moves
  case class L() extends Moves
  case class R() extends Moves

