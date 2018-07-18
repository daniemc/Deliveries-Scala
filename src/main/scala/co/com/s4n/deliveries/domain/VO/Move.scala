package co.com.s4n.deliveries.domain.VO

sealed trait Move extends Product with Serializable
case class A() extends Move
case class L() extends Move
case class R() extends Move
