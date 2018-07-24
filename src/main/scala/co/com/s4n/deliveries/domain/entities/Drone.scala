package co.com.s4n.deliveries.domain.entities

import co.com.s4n.deliveries.infrastructure.FileAccess
import co.com.s4n.deliveries.infrastructure.FileAccess.droneOutput

case class Drone(name: String, position: Position)

object Drone {
  def reportDeliver(drone: Drone): Drone = {
    val message = s"Delivery: (${drone.position.x}, ${drone.position.y} - ${drone.position.o})"
    FileAccess.write(droneOutput(drone.name), message)
    drone
  }
}
