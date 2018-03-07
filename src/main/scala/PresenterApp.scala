package com.encranion.present

import akka.actor.{Actor, ActorSystem, Props, ActorRef, PoisonPill}
import scala.concurrent.duration._
import com.typesafe.config.ConfigFactory
import scala.io.{Source, StdIn}
import java.nio.file.{Files, Path, Paths}


case object RunBlock

trait EventWriter {
  def markEvent(i : Long) : Unit
}

class PrintLnEventWriter extends EventWriter {
  def markEvent(i : Long) = println(i)
}

class PrintTimeActor(val eventWriter : EventWriter, val next : ActorRef) extends Actor {
  def receive = {
    case RunBlock => {
      eventWriter.markEvent(System.currentTimeMillis())
      next ! RunBlock
    }
  }
}

class FooPrinter extends Actor {
  def receive = {
    case RunBlock => println("foo")
  }
}

object PresenterApp {

  def main(args : Array[String]) : Unit = {
    if (args.length < 3) {
      println(
        """Not enough input arguments.
        The first argument must be the output directory
        The second argument must be the experiment definition file.
        The third argument is the serial port name (COM4 maybe)""")
      System.exit(0)
    }

    val outputFolderPath: Path = Paths.get(args(0))
    val expDefPath: Path = Paths.get(args(1))
    val portName : String = args(2)

    if (!Files.exists(expDefPath)){
      println(s"Experiment definition file does not exist.")
      System.exit(0)
    }

    if (!Files.isDirectory(outputFolderPath)) {
      println(s"Output folder doesn't exist")
      System.exit(0)
    }

    val timingOutputPath = outputFolderPath.resolve("timing.txt")
    val responseOutputPath = outputFolderPath.resolve("responses.txt")

    if (Files.exists(timingOutputPath) || Files.exists(responseOutputPath)) {
      println(s"""Output files already exist.\n
          Type 'overwrite' and press enter if you want to overwrite files\n
          Type 'quit' and press enter to quit""")
      val ret = StdIn.readLine()
      if (ret != "overwrite") System.exit(0)
    }

    val lines = Source.fromFile(expDefPath.toFile).getLines.toVector
    val expDef = ExperimentDefinition.fromStrings(lines)

    val eventWriter = new PrintLnEventWriter
    val imageIconStore = StimuliStore.buildImageIconStore(expDef.stimuli)

    val system = akka.actor.ActorSystem.create("presenterRoot")
    try {
      val experimentActor = system.actorOf(Props(classOf[BlockingSwingExperimentSupervisor], expDef, eventWriter, imageIconStore))
      experimentActor ! StartExperiment
    } catch {
      case e : Throwable => {
        system.terminate
        println(e.getMessage)
        System.exit(0)
      }
    }
    /*
    val plWriter = new PrintLnEventWriter
    val system = akka.actor.ActorSystem.create("presenterRoot")
    val fooPrinterRef = system.actorOf(Props(classOf[FooPrinter]))
    val printTimeRef = system.actorOf(Props(classOf[PrintTimeActor], plWriter, fooPrinterRef))
    /*import system.dispatcher
    val schedule = system.scheduler.schedule(0 milliseconds, 1 milliseconds, printTimeRef, RunBlock)
    Thread.sleep(20)
    schedule.cancel
    */
    for( i <- (0 to 100)) {
      printTimeRef ! RunBlock
      Thread.sleep(1)
    }
    system.terminate
    */
  }

}
