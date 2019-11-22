// Wrapping up: Mnemonics for Phone Numbers
import scala.io.Source

val home = System.getProperty("user.home")
val wordsSource = s"${home}/Documents/projects/progfun/code-notes/src/main/scala/week6/linuxwords.txt"
val in = Source.fromFile(wordsSource)
val words = in.getLines.toList filter (word => word forall (chr => chr.isLetter))
in.close()

val mnem = Map(
  '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
  '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

val charCode: Map[Char, Char] =
  for ((digit, str) <- mnem; ltr <- str) yield ltr -> digit

def wordCode(word: String): String =
  word.toUpperCase map charCode

wordCode("Java")

val wordsForNum: Map[String, Seq[String]] =
  words groupBy wordCode withDefaultValue Seq()

def encode(number: String): Set[List[String]] =
  if (number.isEmpty) Set(List())
  else {
    for {
      split <- 1 to number.length
      word <- wordsForNum(number take split)
      // ^ so if no words (eg wordsForNum("7") as no single
      // letter word corresponding to 7, nothing to iterate over
      // (empty List), so goes to next split value!
      rest <- encode(number drop split)
    } yield word :: rest
  }.toSet

encode("7225247386")

def translate(number: String): Set[String] =
  encode(number) map (_ mkString " ")

translate("7225247386")
