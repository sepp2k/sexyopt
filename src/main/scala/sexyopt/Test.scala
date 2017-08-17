package sexyopt

private object Test extends SexyOpt {
    override val programName = "test"
    override val programDescription = "A test program to test option parsing."

    val fileName = posArg("filename", "The name of the file to ignore")
    // val stuff = restArgs("stuff", "Other stuff", atLeastOne = false)
    // val stuff = restArgs("stuff", "Other stuff", atLeastOne = true)
    // val stuff = optionalPosArg("stuff", "Other stuff")
    val stuff = optionalPosArg("stuff", "Other stuff", "default stuff")
    val someOption = option("some-option", 's',
        """Some option, which has a very long description with paragraphs and everything.
        |
        |Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt.""".stripMargin)
    val someDef = option("some-def", "Some option with default", default = "the default")
    val aFlag = flag("a-flag", 'f', "A very important flag")
    val anotherFlag = flag("another-flag", "A less important flag")

    def main(args: Array[String]) = {
        parse(args)
        println(s"fileName = $fileName")
        println(s"stuff = $stuff")
        println(someOption.map("someOption = " + _).getOrElse("someOption has not been provided"))
        println(s"someDef = $someDef")
        if (aFlag) println("aFlag has been provided")
        else println("aFlag has not been provided")
        println(s"anotherFlag = anotherFlag")
    }
}