package co.com.s4n.deliveries

object Config {
  val rootPath = System.getProperty("user.dir")
  val basePath = "src/main/resources"
  def fullPath = {
    s"${ rootPath }/${ basePath }"
  }
}
