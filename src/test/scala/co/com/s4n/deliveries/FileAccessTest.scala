package co.com.s4n.deliveries
import co.com.s4n.deliveries.infrastructure.FileAccess
import org.scalatest._

import scala.util.Try

class FileAccessTest extends FunSuite {
  test("can read a file") {
    val file = FileAccess.read("in.txt")
    assert(file.isSuccess)
  }

  test("if a bad file name is given should fail") {
    val file = FileAccess.read("badFile.txt")
    assert(file.isFailure)
  }

  test("can write a file") {
    val write = FileAccess.write("test.txt", "test message")
    assert(write.isSuccess)
  }

  test("writing a file should fail when i don't give a name") {
    val write = FileAccess.write("", "test message")
    assert(write.isFailure)
  }

  test("can list files in a directory") {
    val files = FileAccess.list(Config.fullPath)
    assert(files.isSuccess)
    files.map(fileList => assert(0 < fileList.length))
  }
}
