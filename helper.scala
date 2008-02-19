import java.lang.reflect._
import java.io._

object helper {
  def ? (obj : AnyRef, verbose : Boolean) = {
    val objClass = obj.getClass
    val methods = obj.getClass.getMethods
    val publicMethods = methods.filter(m => (m.getModifiers & Modifier.PUBLIC) == Modifier.PUBLIC)
    println (objClass.getCanonicalName + "\n------------")
    if (verbose) {
      publicMethods.foreach(m => println (m))
    } else {
      publicMethods.foreach(m => println (m.getName))
    }
  }

  def ? (obj : AnyRef) : Any = {
    ? (obj, false)
  }

  def ?? (obj : AnyRef) = {
    ? (obj, true)
  }

  def exec (cmd : String) = {
    val runTime = Runtime.getRuntime   
    val process = runTime.exec (cmd)
    val resultBuffer = new BufferedReader(new InputStreamReader(process.getInputStream))
    var line : String = null
    var lineList : List[String] = Nil


    do {
      line = resultBuffer.readLine
      if (line != null) {
        lineList = line :: lineList
      }
    } while (line != null)

    process.waitFor

    (process.exitValue, lineList.reverse)
  }
}
