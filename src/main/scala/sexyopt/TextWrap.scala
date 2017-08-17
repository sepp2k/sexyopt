package sexyopt

object TextWrap {
    /**
      * Split the string `text` into an array of lines that are at most `width` characters long (not counting the `\n`).
      *
      * Each line in the array will end with a newline character. There will be no spaces at the end of a line. The string
      * may already contain linebreaks and those will be preserved. The string must not contain words longer than 80
      * characters or multiple consecutive whitespace characters.
      */
    def wrap(text: String, width: Int): Seq[String] = {
        if (text.isEmpty) Seq()
        else {
            val (line, rest) = takeLine(text, width)
            line +: wrap(rest, width)
        }
    }

    /**
      * Extract one line of at most `width` characters from the given string.
      *
      * @return A pair containing the extracting line and the remaining string.
      */
    def takeLine(str: String, width: Int): (String, String) = {
        val newlineIdx = str.indexOf('\n')
        if (newlineIdx >= 0 && newlineIdx <= width) splitAt(str, newlineIdx)
        else if(str.length <= width) (str, "")
        else {
            var current = 0
            var previous = -1
            while (current >= 0 && current <= width) {
                previous = current
                current = str.indexOf(' ', current + 1)
            }
            val (line, rest) = splitAt(str, previous)
            (line.replaceFirst(" $", "\n"), rest)
        }
    }

    /**
      * Split the given string into two after the given index.
      *
      * The first string will include everything up to and including the given index and the second string will contain
      * the rest of the string.
      */
    def splitAt(str: String, idx: Int) = (str.substring(0, idx + 1), str.substring(idx + 1))
}
