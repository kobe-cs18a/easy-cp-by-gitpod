
import jp.kobe_u.copris._
import jp.kobe_u.copris.dsl._
object Main {
    def main(args: Array[String]) {
        println("Hello")
    }
}

object DSL {
    def load(fileName: String) = {
        (new loader.SugarLoader(csp)).load(fileName)
    }
}