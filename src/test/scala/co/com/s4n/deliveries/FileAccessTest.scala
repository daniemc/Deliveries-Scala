package co.com.s4n.deliveries
import co.com.s4n.deliveries.infrastructure.FileAccess
import org.scalatest._

import scala.util.Try

class FileAccessTest extends FunSuite {
  test("can read a file") {
    val file = Try(FileAccess.read("in.txt"))
    assert(file.isSuccess)
  }

  test("if a bad file name is given should fail") {
    val file = Try(FileAccess.read("badFile.txt"))
    assert(file.isFailure)
  }

  test("can write a file") {
    val write = Try(FileAccess.write("test.txt", "test message"))
    assert(write.isSuccess)
  }

  test("writing a file should fail when i don't give a name or message") {
    val write = Try(FileAccess.write("", "test message"))
    val write2 = Try(FileAccess.write("text.txt", ""))
    assert(write.isFailure)
    assert(write2.isFailure)
  }

  test("can list files in a directory") {
    val files = Try(FileAccess.list)
    assert(files.isSuccess)
    files.map(fileList => assert(0 < fileList.length))
  }
}
