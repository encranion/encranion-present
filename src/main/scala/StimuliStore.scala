package com.encranion.present

import javax.swing.ImageIcon
import javax.sound.sampled.{Clip, AudioSystem, DataLine, LineEvent, LineListener}
import java.io.File
import scala.collection.mutable.{Map => MutMap}

object ImageIconUtils{
  def createIfExists(p : String) : ImageIcon = {
    if (!(new File(p)).exists) throw new InvalidStimulusException(s"Image $p doesnt exist")
    val icon = new ImageIcon(p)
    if (icon.getIconHeight == -1) throw new InvalidStimulusException(s"Something is wrong with the image at location $p")
    icon
  }
}

class ImageIconStore(stimuli : Vector[ImageStimulus]) {
  val uniqueStim : Map[String, ImageIcon] = stimuli.map(_.path).toSet.map{ p : String =>
    (p, ImageIconUtils.createIfExists(p))
  }.toMap
  def get(stimulus : ImageStimulus) : ImageIcon = uniqueStim(stimulus.path)
}

class AudioClipStore(stimuli : Vector[SoundStimulus]){
  val uniqueStim : Map[String, Clip] = stimuli.map(_.path).toSet.map{p : String => {
    val sound = AudioSystem.getAudioInputStream(new File(p))
    //val info = new DataLine.Info(classOf[Clip], sound.getFormat)  // DataLine.Info
    //val clip : Clip = AudioSystem.getLine(info).asInstanceOf[Clip]
    val clip : Clip = AudioSystem.getClip()
    clip.open(sound)
    /*
    clip.addLineListener(new LineListener() {
      def update(event: LineEvent) {
        if (event.getType == LineEvent.Type.STOP) {
          event.getLine.close
        }
      }
    })
    */
    (p, clip)
  }}.toMap
  def get(stimulus : SoundStimulus) : Clip = {
    val clip = uniqueStim(stimulus.path)
    clip.setMicrosecondPosition(stimulus.startTimeMillis * 1000)
    clip
  }
}

class ResponseImageIconStore(stimuli : Vector[ResponseStimulus]){
  val uniquePrompts : Map[String, ImageIcon] = stimuli.map(_.promptPath).toSet.map{ p : String =>
    (p, ImageIconUtils.createIfExists(p))
  }.toMap

  val mutableMap : MutMap[String,Map[Int,String]] = MutMap[String,Map[Int, String]]()
  for(stimulus <- stimuli){
    mutableMap.get(stimulus.promptPath) match {
      case Some(optMap) => {
        if (optMap != stimulus.confirmPrompts) {
          throw new InvalidStimulusException("Responses with the same prompt must have the same option code and confirm prompts.")
        }
      }
      case None => {
        mutableMap += ((stimulus.promptPath, stimulus.confirmPrompts))
      }
    }
  }
  val confirmPrompts : Map[String,Map[Int,ImageIcon]] = mutableMap.map{case (p : String, optMap : Map[Int,String]) =>
    (p, optMap.map{case (i : Int, cp : String) => (i, ImageIconUtils.createIfExists(cp))})}.toMap

  def getPrompt(stimulus : ResponseStimulus) : ImageIcon = uniquePrompts(stimulus.promptPath)
  def getOptionResponsePrompt(stimulus : ResponseStimulus, option : Int) : ImageIcon = {
    confirmPrompts(stimulus.promptPath)(option)
  }
}

object StimuliStore {

  def buildImageIconStore(stimuli : Vector[Stimulus]) = new ImageIconStore(stimuli.flatMap {
    case s: ImageStimulus => Some(s)
    case _ => None
  })
  def buildAudioClipStore(stimuli : Vector[Stimulus]) = new AudioClipStore(stimuli.flatMap{
    case s: SoundStimulus => Some(s)
    case _ => None
  })
  def buildResponseImageIconStore(stimuli : Vector[Stimulus]) = new ResponseImageIconStore(stimuli.flatMap{
    case s: ResponseStimulus => Some(s)
    case _ => None
  })

}
