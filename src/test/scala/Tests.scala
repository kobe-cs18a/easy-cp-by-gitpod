import org.scalatest.FunSuite

import org.scalatest.Reporter
import org.scalatest.events._
import org.scalatest.Tag
import Console._

class SampleReporter extends Reporter {
  // https://qiita.com/NomadBlacky/items/335cc84d0da978240b60

  var totalScore = 0
  var fullScore = 0
  var table = IndexedSeq.empty[String]
  val c1width = 30
  val c2width = 3
  val c3width = 3

  private def alignR(str: String, width: Int) = {
    val strWidth = str.map{ s => 
        if (s.toString.getBytes(java.nio.charset.Charset.forName("UTF-8")).length > 1) 2 else 1
      }.sum
    s"${Seq.fill(width - strWidth)(" ").mkString}$str"
  }
  
  private def alignL(str: String, width: Int) = {
    val strWidth = str.map{ s => 
        if (s.toString.getBytes(java.nio.charset.Charset.forName("UTF-8")).length > 1) 2 else 1
      }.sum
    s"$str${Seq.fill(width - strWidth)(" ").mkString}"
  }

  override def apply(event: Event): Unit = event match {
    case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, duration, formatter, location, rerunner, payload, threadName, timeStamp) => {
        val name = testName.split(':').head.trim
        val score = testName.split(':').last.trim.toInt
        totalScore += score
        fullScore += score
        table = table :+ s"| ${alignL(s"${suiteName}-${name}", c1width)} | ${alignR(score.toString, 3)} |" + GREEN + " OK " + RESET + s"| ${alignR(score.toString, 3)} |"
        // println(s"OK: ${suiteName} ${testName}")
    }
    case TestFailed(ordinal, message, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, analysis, throwable, duration, formatter, location, rerunner, payload, threadName, timeStamp) => {
        val name = testName.split(':').head.trim
        val score = testName.split(':').last.trim.toInt
        fullScore += score
        table = table :+ s"| ${alignL(s"${suiteName}-${name}", c1width)} | ${alignR(score.toString, 3)} |" + RED + " NG " + RESET + s"| ${alignR("0", 3)} |"
    }
    case RunCompleted(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) => {
        table.sorted.foreach(println)
        println("|" + Seq.fill(c1width+c2width+c3width+13)("-").mkString + "|")
        println(s"| ${alignL(s"Total", 30)} | ${alignR(fullScore.toString, 3)} |    | ${alignR(totalScore.toString, 3)} |")
    }
    case _ =>
}
    /*     case _: RecordableEvent =>
    case _: ExceptionalEvent =>
    case _: NotificationEvent =>
    case TestStarting(ordinal, suiteName, suiteId, suiteClassName, testName, testText, formatter, location, rerunner, payload, threadName, timeStamp) =>
    case TestFailed(ordinal, message, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, analysis, throwable, duration, formatter, location, rerunner, payload, threadName, timeStamp) =>
    case TestIgnored(ordinal, suiteName, suiteId, suiteClassName, testName, testText, formatter, location, payload, threadName, timeStamp) =>
    case TestPending(ordinal, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, duration, formatter, location, payload, threadName, timeStamp) =>
    case TestCanceled(ordinal, message, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, throwable, duration, formatter, location, rerunner, payload, threadName, timeStamp) =>
    case SuiteStarting(ordinal, suiteName, suiteId, suiteClassName, formatter, location, rerunner, payload, threadName, timeStamp) =>
    case SuiteCompleted(ordinal, suiteName, suiteId, suiteClassName, duration, formatter, location, rerunner, payload, threadName, timeStamp) =>
    case SuiteAborted(ordinal, message, suiteName, suiteId, suiteClassName, throwable, duration, formatter, location, rerunner, payload, threadName, timeStamp) =>
    case RunStarting(ordinal, testCount, configMap, formatter, location, payload, threadName, timeStamp) =>
    case RunStopped(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) =>
    case RunAborted(ordinal, message, throwable, duration, summary, formatter, location, payload, threadName, timeStamp) =>
    case InfoProvided(ordinal, message, nameInfo, throwable, formatter, location, payload, threadName, timeStamp) =>
    case AlertProvided(ordinal, message, nameInfo, throwable, formatter, location, payload, threadName, timeStamp) =>
    case NoteProvided(ordinal, message, nameInfo, throwable, formatter, location, payload, threadName, timeStamp) =>
    case MarkupProvided(ordinal, text, nameInfo, formatter, location, payload, threadName, timeStamp) =>
    case ScopeOpened(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
    case ScopeClosed(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
    case ScopePending(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
    case DiscoveryStarting(ordinal, configMap, threadName, timeStamp) =>
    case DiscoveryCompleted(ordinal, duration, threadName, timeStamp) =>
 */  
}

class A_Test01 extends FunSuite {

    test(s"テスト用 always true: 2") {
        assert(1 == 1)
    }

}

class A_Test02 extends FunSuite {

    test(s"always true: 4") {
        assert(1 == 1)
    }

}

class A_Test03 extends FunSuite {

    test(s"always false: 4") {
        assert(1 == 0)
    }

}

class B_Test02 extends FunSuite {

    val s = 8
    test(s"always true: 8") {
        assert(1 == 1)
    }

}

class B_Test03 extends FunSuite {

    val s = 8
    test(s"always true: 8") {
        assert(1 == 1)
    }

}