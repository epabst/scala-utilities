package scala_utilities

import java.lang.reflect._
import tools.nsc.io.Path
import tools.nsc.io.Process
import java.lang.{Process => JProcess}
import java.io._
import io.Source

object Helper {
  var currDir : Option[File] = None

  val runTime = Runtime.getRuntime   

  def showClass [T] (inputClass : java.lang.Class[T], verbose : Boolean, indent : Int) : Unit = {
      val methods = inputClass.getDeclaredMethods
      val publicMethods = methods.filter(m => Modifier.isPublic(m.getModifiers))

      val className = inputClass.getCanonicalName

      val indentString = (0 until indent).foldLeft("")((x,y) => x +"  ")

      println (indentString + className + "\n" + indentString + "------------")

      if (verbose) {
        publicMethods.foreach(m => println (indentString + m.toString.replaceAll(className + "\\.", "")))
        val publicFields = inputClass.getDeclaredFields.filter(f => Modifier.isPublic(f.getModifiers))
        println (indentString + "---Public Fields---")
        publicFields.foreach(f => println (indentString + f))
        if (inputClass.getSuperclass != null) {
            println("")
            showClass (inputClass.getSuperclass, verbose, indent + 1)
        }
      } else {
        val names = publicMethods.map(m => m.getName).toList.distinct
        names.foreach(name => println (indentString + name + "( )"))
      }

      
  }

  def ? (obj : AnyRef, verbose : Boolean) = {
      val classToShow = obj.getClass
      showClass(classToShow, verbose, 0)
  }

  def ? (obj : AnyRef) : Any = {
    ? (obj, false)
  }

  def ?? (obj : AnyRef) = {
    ? (obj, true)
  }

  private def printAllLines(inputStream: InputStream) {
    try for (line <- Source.fromInputStream(inputStream).getLines) {
      println(line)
    } catch { case _: IOException => () }
  }

  def exec (cmd : String): Int = {
    val process = if (currDir.isDefined) Process(cmd, null, Path(currDir.get)) else Process(cmd)
    val jProcess: JProcess = process.process
    printAllLines(jProcess.getInputStream)

    process.waitFor
    process.exitValue.get
  }

  def execp (cmd : String): (Int, List[String]) = {
    val process = if (currDir.isDefined) Process(cmd, null, Path(currDir.get)) else Process(cmd)
    val lineList = process.stdout.toList

    process.waitFor

    val errList = process.stderr.toList
    (process.exitValue.get, lineList ::: errList)
  }

  def execp (cmd : String, outFile:String): Int = {
    val process = if (currDir.isDefined) Process(cmd, null, Path(currDir.get)) else Process(cmd)
    val outputWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outFile)))
    for (line <- process.stdout) {
      outputWriter.write(line)
      outputWriter.newLine
    }

    process.waitFor
    outputWriter.close

    process.exitValue.get
  }

  def cwd (dir : String) = {
    currDir = Some(new File(dir))
  }

  def exists(name : String) = {
    (new File(name)).exists
  }

  case class hex(value : Int) { override def toString = "0x" + value.toHexString }
}
