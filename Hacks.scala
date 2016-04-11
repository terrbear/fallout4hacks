
import scala.annotation.tailrec
import scala.util.Random

object Hacks {
  def main(args: Array[String]) : Unit = {
    args.map(x => (x.size, x)).toMap match {
      case x if x.keys.size == 1 =>
      case mismatch => {
        println(s"One of your words isn't the right length $mismatch")
        System.exit(1)
      }
    }

    val terminal = new Terminal(args.toSeq)
    println(s"final answer: ${terminal.guess}")
  }
}

class Terminal(words: Seq[String]) {
  val notes = scala.collection.mutable.Map.empty[String, Int]
  val wordSize = words.head.size

  def guess : String = {
    val s = sample
    println(s"guessing: $s")
    val right = scala.math.min(scala.io.StdIn.readLine("How many right? ").toInt, words.head.size)
    notes(s) = right
    right match {
      case ws if ws == wordSize => s
      case _ => guess
    }
  }

  def overlaps = {
    val filtered = filteredWords
    filtered.map { word =>
      (filtered.foldLeft(0) { (sum, otherWord) =>
        otherWord match {
          case ow if ow == word => sum
          case _ => sum + intersect(word, otherWord).size
        }
      }, word)
    }.toMap
  }

  def sample : String = {
    val overlap = overlaps
    overlap match {
      case e if e.isEmpty => {
        println("Seems like you mistyped a word")
        System.exit(1)
        "Unreached"
      }
      case _ => overlaps(overlaps.keys.max)
    }
  }

  def sampleRandom = {
    Random.shuffle(filteredWords.toList).take(1).headOption match {
      case Some(s) => s.toUpperCase
      case None => {
        println("Seems like you mistyped a word")
        System.exit(1)
      }
    }
  }

  def filteredWords = {
    words.filter{word =>
      notes.foldLeft(true){
        case (matches, (guessed, count)) =>
          matches && intersect(word, guessed).size == count
      }
    }
  }

  def intersect(str1:String, str2:String) = gameify(str1).intersect(gameify(str2))
  def gameify(str: String) = str.toUpperCase.split("").drop(1).zipWithIndex.toSet
}
