package com.encranion.present

import akka.actor.{Actor, ActorRef, PoisonPill, Props}
import javax.swing.{JFrame, WindowConstants, JLabel}
import java.awt.{Frame, Color}
import java.awt.event.WindowEvent
import scala.collection.mutable.ArrayBuffer

trait ExperimentAction
case object StartExperiment extends ExperimentAction
case object EndExperiment extends ExperimentAction

trait StimulusAction
case object PresentStimulus extends StimulusAction

trait BlockingActor extends Actor


trait PresenterActor extends Actor
trait ImagePresenterActor extends PresenterActor {
  val stimulus : ImageStimulus
}
trait SoundPresenterActor extends PresenterActor {
  val stimulus : SoundStimulus
}
trait BlankPresenterActor extends PresenterActor {
  val stimulus : BlankStimulus
}
trait ResponsePresenterActor extends PresenterActor {
  val stimulus : ResponseStimulus
}

trait HasNextActor extends PresenterActor {
  val next : ActorRef
}
trait BlockingPresenterActor extends PresenterActor
trait BlockingSwingPresenterActor extends BlockingPresenterActor {
  val jFrame : JFrame
}

class BlockingSwingImageIconPresenterActor(val stimulus : ImageStimulus,
                                           val store : ImageIconStore,
                                           val jFrame : JFrame,
                                           val marker : EventWriter,
                                           val next : ActorRef)
  extends BlockingSwingPresenterActor with HasNextActor {

  def receive = {
    case PresentStimulus => {
      println(s"Presenting $stimulus")
      val image = store.get(stimulus)
      val label = new JLabel(image)
      label.setBounds(0,0, image.getIconWidth, image.getIconHeight)
      label.setVisible(true)
      jFrame.add(label)
      jFrame.setVisible(true)
      jFrame.repaint()
      Thread.sleep(stimulus.durationInMillis)
      jFrame.remove(label)
      label.setVisible(false)
      jFrame.repaint()
      next ! PresentStimulus
      self ! PoisonPill
    }
  }
}


class BlockingSoundPresenterActor(val stimulus : SoundStimulus,
                                  val store : AudioClipStore,
                                  val marker : EventWriter,
                                  val next : ActorRef)
  extends BlockingPresenterActor with HasNextActor {

  def receive = {
    case PresentStimulus => {
      println(s"Presenting $stimulus")
      Thread.sleep(stimulus.endTimeMillis - stimulus.startTimeMillis)
      next ! PresentStimulus
      self ! PoisonPill
    }
  }
}

class BlockingBlankPresenterActor(val stimulus : BlankStimulus,
                                  val marker : EventWriter,
                                  val next : ActorRef)
  extends BlockingPresenterActor with HasNextActor {

  def receive = {
    case PresentStimulus => {
      println(s"Presenting $stimulus")
      Thread.sleep(stimulus.durationInMillis)
      next ! PresentStimulus
      self ! PoisonPill
    }
  }
}

class EndExperimentActor(val next : ActorRef) extends HasNextActor {
  def receive = {
    case PresentStimulus => {
      next ! EndExperiment
      self ! PoisonPill
    }
  }
}



abstract class ExperimentSupervisor(val expDef : ExperimentDefinition) extends Actor


class BlockingSwingExperimentSupervisor(override val expDef : ExperimentDefinition,
                                        val eventWriter : EventWriter, val imageIconStore : ImageIconStore)
  extends ExperimentSupervisor(expDef){

    val jFrame = new JFrame("experiment")
  jFrame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
  jFrame.getContentPane().setBackground(Color.BLACK)
  jFrame.setUndecorated(true);

  jFrame.setExtendedState(Frame.MAXIMIZED_BOTH);
  jFrame.setUndecorated(true);
  jFrame.pack();
  jFrame.setVisible(true);

  val lastActor = context.actorOf(Props(classOf[EndExperimentActor], self))

  val firstActor = expDef.stimuli.foldRight(lastActor) {
    (s: Stimulus, nextActor: ActorRef) =>
      s match {
        case is: ImageStimulus =>
          context.actorOf(Props(classOf[BlockingSwingImageIconPresenterActor],
            is, imageIconStore, jFrame, eventWriter, nextActor))
        case bs: BlankStimulus => context.actorOf(Props(classOf[BlockingBlankPresenterActor], bs, eventWriter, nextActor))
        case _ => ???
      }
  }

  def receive = {
    case StartExperiment => {
      firstActor ! PresentStimulus
    }
    case EndExperiment =>  {
      println("Ending experiment")
      jFrame.dispose()
      context.parent ! PoisonPill
      self ! PoisonPill
    }
  }

  override def postStop() = context.parent ! PoisonPill
}


