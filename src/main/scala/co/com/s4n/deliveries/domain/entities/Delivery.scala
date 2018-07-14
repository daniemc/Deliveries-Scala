package co.com.s4n.deliveries.domain.entities

import co.com.s4n.deliveries.domain.VO.Moves

import scala.util.Try

case class Delivery(route : List[Try[Moves]])
