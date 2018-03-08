package com.encranion.present

final case class InvalidStimulusException(private val message : String) extends Exception(message)
trait Stimulus
case class ImageStimulus(marker : Int, durationInMillis : Long,  path : String) extends Stimulus
case class SoundStimulus(marker : Int, startTimeMillis : Long, endTimeMillis : Long, path : String) extends Stimulus
case class BlankStimulus(marker : Int, durationInMillis : Long) extends Stimulus
case class ResponseStimulus(marker : Int, promptPath : String, confirmPromptDurationInMillis : Long, confirmPrompts : Map[Int, String]) extends Stimulus



class ExperimentDefinition(val stimuli : Vector[Stimulus]) {

}

object ExperimentDefinition {
  private val ImageKey = "IMAGE"
  private val SoundKey = "SOUND"
  private val BlankKey = "BLANK"
  private val ResponseKey = "RESPONSE"


  def parseImageParams(params : Vector[String]) : ImageStimulus = {
    try {
      ImageStimulus(params(0).toInt, (params(1).toDouble * 1000).toLong, params(2))
    } catch {
      case _ : Throwable => throw new InvalidStimulusException("Something is incorrect")
    }
  }

  def parseBlankParams(params : Vector[String]) : BlankStimulus = {
    try {
      BlankStimulus(params(0).toInt, (params(1).toDouble * 1000).toLong)
    } catch {
      case _ : Throwable => throw new InvalidStimulusException("Something is incorrect")
    }
  }

  def parseSoundParams(params : Vector[String]) : SoundStimulus = {
    try {
      SoundStimulus(params(0).toInt,
        (params(1).toDouble * 1000).toLong,
        (params(2).toDouble * 1000).toLong,
        params(3))
    } catch {
      case _ : Throwable => throw new InvalidStimulusException("Something is incorrect")
    }
  }

  def parseResponseParams(params : Vector[String]) : ResponseStimulus = {
    try {
      val marker = params(0).toInt
      val promptPath = params(1)
      val confirmDur = (params(2).toDouble * 1000).toLong
      val options = params.drop(3)
      val nOptions = options.length / 2
      val keyCodes : Seq[Int] = (0 until nOptions).map(i => options(i*2).toInt)
      val optionPrompts : Seq[String] = (0 until nOptions).map(i => options((i*2)+1))
      val optMap : Map[Int, String] = keyCodes.zip(optionPrompts).toMap

      ResponseStimulus(marker, promptPath, confirmDur, optMap)
    } catch {
      case _ : Throwable => throw new InvalidStimulusException("Something is incorrect")
    }
  }

  def fromStrings(items : Vector[String]) : ExperimentDefinition = {
    val stimulus : Vector[Stimulus] = items.map { l =>
      val split = l.split(" ")
      val stimTypeKey = split(0)
      val stimParams : Vector[String] = split.drop(1).toVector
      try {
      stimTypeKey match {
          case ImageKey => parseImageParams(stimParams)
          case SoundKey => parseSoundParams(stimParams)
          case ResponseKey => parseResponseParams(stimParams)
          case BlankKey => parseBlankParams(stimParams)
          case _ => throw new InvalidStimulusException("First item should be the stimulus type")
        }

      } catch {
        case InvalidStimulusException(m : String) =>
          throw new InvalidStimulusException(s"$m in stimulus defintion: $l")
      }
    }
    return new ExperimentDefinition(stimulus)
  }
}

