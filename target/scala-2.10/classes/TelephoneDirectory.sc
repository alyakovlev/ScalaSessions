val in = scala.io.Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")

val words: List[String] = in.getLines().toList filter (w => w forall (c => c.isLetter))

val mnemonics: Map[Char, String] = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

/*
  Получаем на основе карты `mnemonics` отношение Цифра -> Символ
 */
val charCode: Map[Char, Char] = for {
  (number, str) <- mnemonics
  s <- str
} yield s -> number

/*
  Кодируем word в набор цифр
 */
def wordCode(word: String): String = word.toUpperCase map charCode

wordCode("Scalamnemonics")

/*
  Преобразуем словарь `words` в отношение код -> варианты слов, кодируемые этим кодом
 */
val wordsForNum: Map[String, Seq[String]] = words groupBy wordCode withDefaultValue List()

wordsForNum("72252")
wordsForNum("663666427")

/*
  Получаем на основе полученного кода `numbers` набор всех возможных вариантов предложений
  Подсказка: используйте for-expression
 */
def encode(number: String): Set[List[String]] =
  if (number.isEmpty) Set(List())
  else {
    for {
      splitIndex <- 1 to number.length
      w <- wordsForNum(number take splitIndex)
      tail <- encode(number drop splitIndex)
    } yield w :: tail
  }.toSet

encode("72252663666427")


def translate(number: String): Set[String] = encode(number) map (_ mkString " ")

translate("72252663666427").contains("Scala mnemonics")