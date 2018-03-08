package com.encranion.present

import scala.io.{Source, StdIn}
import java.nio.file.{Files, Path, Paths}
import java.nio.ByteBuffer
import java.io.{BufferedWriter, FileWriter, OutputStream, Writer}
import purejavacomm._

import akka.actor.{ActorSystem, Props}


case object RunBlock

trait EventWriter {
  def markEvent(i : Int) : Unit
}

trait ResponseWriter{
  def markRespose(rs : ResponseStimulus, option : Int)
}

class MultiEventWriter(val eventWriters : Vector[EventWriter]) extends  EventWriter {
  def markEvent(i : Int) = {
    eventWriters.foreach(_.markEvent(i))
  }
}

class PrintLnEventWriter extends EventWriter {
  def markEvent(i : Int) = println(i)
}

class WriterEventWriter(val writer : Writer) extends EventWriter {
  def markEvent(i : Int) = {
    val time = System.currentTimeMillis()
    writer.write(s"$time $i\n")
    writer.flush()
  }
}

class WriterResponseWriter(val writer : Writer) extends ResponseWriter {
  override def markRespose(rs: ResponseStimulus, option: Int): Unit = {
    val time = System.currentTimeMillis()
    writer.write(s"$time $option\n")
    writer.flush()
  }
}

class ESUEventWriter(val portName : String ) extends EventWriter{

  val sp : SerialPort = CommPortIdentifier.getPortIdentifier(portName)
    .open("ESUMarker",1000).asInstanceOf[SerialPort]
  sp.setSerialPortParams(57600, SerialPort.DATABITS_8, SerialPort.STOPBITS_1, SerialPort.PARITY_NONE)
  val outStream : OutputStream = sp.getOutputStream()

  val byteStart : Array[Byte] = Array[Byte](86,90,0,4,1)

  def markEvent(i : Int) : Unit = {
    val dataBytes : Array[Byte] = ByteBuffer.allocate(4).putInt(i).array()
    val dataBytesInt = dataBytes.map{
      b => if (b < 0) 256 + b else b
    }
    val dataBytesSum = dataBytesInt(0) + dataBytesInt(1) + dataBytesInt(2) + dataBytesInt(3)
    val checksum : Array[Byte] = Array((255 - ((5 + dataBytesSum) % 256)).toByte)
    val sendBytes : Array[Byte] = byteStart ++ dataBytes ++ checksum

    outStream.write(sendBytes)
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

    val fileWriter = new WriterEventWriter(new BufferedWriter(new FileWriter(timingOutputPath.toFile)))
    val eventWriter = new MultiEventWriter(Vector(fileWriter))
    val responseWriter = new WriterResponseWriter(new BufferedWriter(new FileWriter(responseOutputPath.toFile)))
    val imageIconStore = StimuliStore.buildImageIconStore(expDef.stimuli)
    val audioClipStore = StimuliStore.buildAudioClipStore(expDef.stimuli)
    val responseImageIconStore = StimuliStore.buildResponseImageIconStore(expDef.stimuli)

    val system = ActorSystem.create("presenterRoot")
    try {
      val experimentActor = system.actorOf(Props(classOf[BlockingSwingExperimentSupervisor],
        expDef, eventWriter, responseWriter, imageIconStore, audioClipStore, responseImageIconStore))
      experimentActor ! StartExperiment
    } catch {
      case e : Throwable => {
        system.terminate
        println(e.getMessage)
        System.exit(0)
      }
    }
  }

}
