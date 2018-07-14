package co.com.s4n.deliveries.domain.entities

import co.com.s4n.deliveries.domain.VO.Orientation

sealed trait PositionBuilder {
  val x: Int
  val y: Int
  val o: Orientation
}

case class Position(x: Int, y: Int, o: Orientation) extends PositionBuilder
