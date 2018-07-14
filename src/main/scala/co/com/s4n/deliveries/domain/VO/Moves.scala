package co.com.s4n.deliveries.domain.VO

sealed trait Moves extends Product with Serializable
case class A() extends Moves
case class L() extends Moves
case class R() extends Moves
case class D() extends Moves

