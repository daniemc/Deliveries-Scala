package co.com.s4n.deliveries.domain.app

sealed trait Orientation
  case class N() extends Orientation
  case class E() extends Orientation
  case class S() extends Orientation
  case class O() extends Orientation