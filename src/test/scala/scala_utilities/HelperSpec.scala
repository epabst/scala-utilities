package scala_utilities

import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

class HelperSpecTest extends JUnit4(HelperSpec)
//class MySpecSuite extends ScalaTestSuite(HelperSpec)
object HelperSpecRunner extends ConsoleRunner(HelperSpec)

object HelperSpec extends Specification {
  "Helper.execp" should {
    "support executing shell commands" in {
      val (exitCode, output) = Helper.execp("echo \"hello\"")
      exitCode must beEqualTo(0)
      output must beEqualTo(List("hello"))
    }
    "command output should preserve order" in {
      val (exitCode, output) = Helper.execp("echo \"hello\"; echo \"howdy\"; echo \"wassup\"")
      exitCode must beEqualTo(0)
      output must beEqualTo(List("hello", "howdy", "wassup"))
    }
    "fail gracefully and capture stderr" in {
      val (exitCode, output) = Helper.execp("echo < foobarbaz")
      exitCode must notBe(0)
      output must notBeEmpty
      output(0) must include("foobarbaz")
    }
  }
  "Helper.exec" should {
    "support executing shell commands" in {
      val exitCode = Helper.exec("echo \"hello\"")
      exitCode must beEqualTo(0)
    }
    "fail gracefully" in {
      val exitCode = Helper.exec("echo < foobarbaz")
      exitCode must notBe(0)
    }
  }
}
