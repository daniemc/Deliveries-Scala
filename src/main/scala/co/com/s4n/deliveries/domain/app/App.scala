package co.com.s4n.deliveries.domain.app

class App {

  sealed trait Moves
  case class A() extends Moves
  case class L() extends Moves
  case class R() extends Moves

  sealed trait Orientation
  case class N() extends Orientation
  case class E() extends Orientation
  case class S() extends Orientation
  case class O() extends Orientation

  trait Position {
    val x = 0
    val y = 0
    val o: Orientation = new N()
  }
}
