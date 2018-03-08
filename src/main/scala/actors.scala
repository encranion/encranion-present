package com.encranion.present

import akka.actor.{Actor, ActorRef, PoisonPill, Props}
import javax.swing.{JFrame, WindowConstants, JLabel}
import java.awt.{Frame, Color}
import java.awt.{KeyEventDispatcher, KeyboardFocusManager}
import java.awt.event.KeyEvent
import java.util.concurrent.CountDownLatch

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
      val image = store.get(stimulus)
      val label = new JLabel(image)
      label.setBounds(0,0, image.getIconWidth, image.getIconHeight)
      label.setVisible(true)
      jFrame.add(label)
      jFrame.setVisible(true)
      jFrame.repaint()
      marker.markEvent(stimulus.marker)
      Thread.sleep(stimulus.durationInMillis)
      jFrame.remove(label)
      label.setVisible(false)
      jFrame.repaint()
      next ! PresentStimulus
      self ! PoisonPill
    }
  }
}

class BlockingResponseSwingImageIconPresenter(val stimulus : ResponseStimulus,
                                              val store : ResponseImageIconStore,
                                              val jFrame : JFrame,
                                              val marker : EventWriter,
                                              val responseWriter : ResponseWriter,
                                              val next : ActorRef)
  extends BlockingSwingPresenterActor with HasNextActor {


  def waitForResponse(keySet : Set[Int]) : Int = {
    var keyPressed : Int = -1
    val latch = new CountDownLatch(1)
    val dispatcher = new KeyEventDispatcher() { // Anonymous class invoked from EDT
      def dispatchKeyEvent(e: KeyEvent): Boolean = {
        if (keySet.contains(e.getKeyCode)) {
          latch.countDown()
          keyPressed = e.getKeyCode
        }
        false
      }
    }
    KeyboardFocusManager.getCurrentKeyboardFocusManager.addKeyEventDispatcher(dispatcher)
    latch.await() // current thread waits here until countDown() is called
    KeyboardFocusManager.getCurrentKeyboardFocusManager.removeKeyEventDispatcher(dispatcher)
    return keyPressed
  }

  def receive = {
    case PresentStimulus => {

      val promptImage = store.getPrompt(stimulus)
      val promptLabel = new JLabel(promptImage)
      promptLabel.setBounds(0,0, promptImage.getIconWidth, promptImage.getIconHeight)
      promptLabel.setVisible(true)
      jFrame.add(promptLabel)
      jFrame.setVisible(true)
      jFrame.repaint()
      marker.markEvent(stimulus.marker)

      val res = waitForResponse(stimulus.confirmPrompts.keySet)
      responseWriter.markRespose(stimulus, res)

      jFrame.remove(promptLabel)
      promptLabel.setVisible(false)

      val confirmImage = store.getOptionResponsePrompt(stimulus,res)
      val confirmLabel = new JLabel(confirmImage)
      confirmLabel.setBounds(0,0, confirmImage.getIconWidth, confirmImage.getIconHeight)
      confirmLabel.setVisible(true)
      jFrame.add(confirmLabel)
      jFrame.setVisible(true)
      jFrame.repaint()
      Thread.sleep(stimulus.confirmPromptDurationInMillis)
      jFrame.remove(confirmLabel)
      confirmLabel.setVisible(false)
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
      val clip = store.get(stimulus)
      clip.start
      marker.markEvent(stimulus.marker)
      Thread.sleep(stimulus.endTimeMillis - stimulus.startTimeMillis)
      clip.stop
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
      marker.markEvent(stimulus.marker)
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
                                        val eventWriter : EventWriter,
                                        val responseWriter : ResponseWriter,
                                        val imageIconStore : ImageIconStore,
                                        val audioClipStore : AudioClipStore,
                                        val responseImageIconStore : ResponseImageIconStore)
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
        case rs : ResponseStimulus =>
          context.actorOf(Props(classOf[BlockingResponseSwingImageIconPresenter],
            rs, responseImageIconStore, jFrame, eventWriter, responseWriter, nextActor))
        case ss : SoundStimulus =>
          context.actorOf(Props(classOf[BlockingSoundPresenterActor], ss, audioClipStore, eventWriter, nextActor))
        case _ => ???
      }
  }

  def receive = {
    case StartExperiment => {
      firstActor ! PresentStimulus
    }
    case EndExperiment =>  {
      jFrame.dispose()
      context.parent ! PoisonPill
      self ! PoisonPill
    }
  }

  override def postStop() = context.parent ! PoisonPill
}


