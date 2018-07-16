package co.com.s4n.deliveries.domain.entities

sealed trait DroneBuilder {
  val name: String  
  val output: String
  val position: Position
}

case class Drone(name: String, output: String,
                 position: Position) extends DroneBuilder
