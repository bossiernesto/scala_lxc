package containers.scala.lxc

import scala.language.implicitConversions
import scala.sys.process._

package object lxc {

  trait ContainerStatus
  case class WorkingContainer(val container: lxcContainer) extends ContainerStatus
  case class BrokenContainer(val container: lxcContainer, descripcion: String) extends ContainerStatus
  case class UnusableContainer(val container: lxcContainer, description: String) extends ContainerStatus
  case class UnexistantContainer(val container: lxcContainer, descripcion: String) extends ContainerStatus

  class ContainerManager {

    def create(name: String, configFile: Option[String] = None,
               template: Option[String] = None,
               backingStore: Option[String] = None,
               templateOptions: Seq[String] = Nil): ContainerStatus = {

      val container = new lxcContainer(name)
      container.create(configFile, template, backingStore, templateOptions)
    }

  }

  /**
   * The lxcContainer is actually the class where the container execution commands will take place.
   */
  class lxcContainer(containerName: String, lastResult: Option[Unit] = None) {

    def executeOn[T](onExisting: Boolean = true)(block: => T): ContainerStatus = {
      if (exists(containerName) == onExisting) {
        val result: T = block
        return WorkingContainer(new lxcContainer(containerName, Some(result)))
      }
      //Check if the container actually exists
      if (onExisting == false)
        return UnexistantContainer(this, "The container(" + containerName + ") does not exist!")
      else
        return UnexistantContainer(this, "The container(" + containerName + ") does not exist!")

    }

    def wrapOptionParameter(option: String, value: Option[String]) = if (value.isEmpty) None else Some(Seq(option, value.get))

    def create(configFile: Option[String] = None,
               template: Option[String] = None,
               backingStore: Option[String] = None,
               templateOptions: Seq[String] = Nil): ContainerStatus = {

      executeOn(false) {
        val cmd = Seq("lxc-create", "-n", containerName)

        val optionals = List(wrapOptionParameter("-f", configFile), wrapOptionParameter("-t", template),
          wrapOptionParameter("-B", backingStore))

        val optionsCmd: Seq[String] = optionals.foldLeft(cmd) { (prev, curr) =>
          curr match {
            case Some(content: Seq[String]) => content
            case None                       => Seq()
          }
        }

        val finalCmd = if (templateOptions.nonEmpty)
          optionsCmd ++ Seq("--") ++ templateOptions
        else
          optionsCmd

        if (finalCmd.! == 0) {
          if (!exists(containerName)) //verify
            return UnexistantContainer(this, "Container " + containerName + " doesn't seem to be created!")
          else
            return UnusableContainer(this, "Creation error of container " + containerName)
        }
      }
    }

    def exists(name: String): Boolean = allAsList contains name

    def allAsList(): Seq[String] = allAsMap.values.flatten.toList

    private def getLXCListProcessStatus(lines: List[String], typeStatus: String, toTypeStatus: String): List[String] = {
      lines.slice(lines.indexWhere(_ == typeStatus) + 1, lines.indexWhere(_ == toTypeStatus))
        .filter(_.nonEmpty)
        .map(_.trim)
    }

    def allAsMap(): Map[String, Seq[String]] = {
      val lines = "lxc-list".!!.split('\n').toList
      val running = getLXCListProcessStatus(lines, "RUNNING", "FROZEN")
      val frozen = getLXCListProcessStatus(lines, "FROZEN", "STOPPED")
      val stopped = lines.slice(lines.indexWhere(_ == "STOPPED") + 1, lines.length)
        .filter(_.nonEmpty)
        .map(_.trim)

      Map("running" -> running, "frozen" -> frozen, "stopped" -> stopped)
    }

    lazy val stopped: Seq[String] =
      allAsMap()("stopped")

    lazy val frozen: Seq[String] =
      allAsMap()("frozen")

    lazy val running: Seq[String] =
      allAsMap()("running")

    //Commands 
    def start(configFile: Option[String] = None): ContainerStatus = {
      executeOn(true) {
        //blocks returns Int
        val cmd = Seq("lxc-start", "-n", containerName, "-d")
        val cmdFinal: Seq[String] = if (!configFile.isEmpty)
          cmd ++ Seq("-f", configFile.get) else cmd
        cmdFinal.!
      }
    }

    def stop(): ContainerStatus = {
      executeOn(true) {
        //blocks returns Int
        Seq("lxc-stop", "-n", containerName).!
      }
    }

    def kill(signal: String): ContainerStatus = {
      executeOn(true) {
        //blocks returns Int
        Seq("lxc-kill", "-n", containerName, signal).!
      }
    }

    def shutdown(wait: Boolean = false, reboot: Boolean = false): ContainerStatus = {
      executeOn(true) {
        //blocks returns Int
        var cmd = Seq("lxc-shutdown", "-n", containerName)
        if (wait) cmd ++= Seq("-w")
        if (reboot) cmd ++= Seq("-r")
        cmd.!
      }
    }

    def destroy(): ContainerStatus = {
      executeOn(true) {
        //blocks returns Int
        Seq("lxc-destroy", "-f", "-n", containerName).!
      }
    }

    def freeze(): ContainerStatus = {
      executeOn(true) {
        //blocks returns Int
        Seq("lxc-freeze", "-n", containerName).!
      }
    }

    def unfreeze(): Int = {
      //blocks returns Int
      Seq("lxc-unfreeze", "-n", containerName).!
    }

    def info(): Map[String, String] = {
      //blocks returns Map
      val cmd = Seq("lxc-info", "--name", containerName)
      Map(cmd.!!.split('\n').map(s => { val x = s.split(':'); (x(0).trim, x(1).trim) }): _*)
    }

    def checkconfig(): String = {
      //blocks returns String
      "lxc-checkconfig".!!
    }

  }

}
