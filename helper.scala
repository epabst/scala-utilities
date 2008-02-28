import java.lang.reflect._
import java.io._

object helper {
  def ? (obj : AnyRef, verbose : Boolean) = {
    val objClass = obj.getClass
    val methods = objClass.getDeclaredMethods
    val publicMethods = methods.filter(m => Modifier.isPublic(m.getModifiers))

    val className = objClass.getCanonicalName

    println (className + "\n------------")

    if (verbose) {
      publicMethods.foreach(m => println (m.toString.replaceAll(className + "\\.", "")))
      val publicFields = objClass.getDeclaredFields.filter(f => Modifier.isPublic(f.getModifiers))
      println ("---Public Fields---")
      publicFields.foreach(f => println (f))
    } else {
      val names = publicMethods.map(m => m.getName).toList.removeDuplicates
      names.foreach(name => println (name + "( )"))
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

    do {
      line = resultBuffer.readLine
      if (line != null) {
        println (line)
      }
    } while (line != null)

    process.waitFor
    process.exitValue
  }

  def execp (cmd : String) = {
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
