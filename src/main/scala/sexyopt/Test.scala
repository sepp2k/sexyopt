package sexyopt

private object Test extends SexyOpt {
    override val programName = "test"
    override val programDescription = "A test program to test option parsing."

    val fileName = posArg("filename", "The name of the file to ignore")
    // val stuff = restArgs("stuff", "Other stuff", atLeastOne = false)
    // val stuff = restArgs("stuff", "Other stuff", atLeastOne = true)
    // val stuff = optionalPosArg("stuff", "Other stuff")
    val stuff = optionalPosArg("stuff", "Other stuff", "default stuff")
    val someOption = option("some-option", 's', "Some option")
    val someDef = option("some-def", "Some option with default", default = "the default")
    val aFlag = flag("a-flag", 'f', "A very important flag")
    val anotherFlag = flag("another-flag", "A less important flag")

    def main(args: Array[String]) = {
        parse(args)
        println(s"fileName = ${fileName.value}")
        println(s"stuff = ${stuff.value}")
        println(s"someOption = ${someOption.value}")
        println(s"someDef = ${someDef.value}")
        println(s"aFlag = ${aFlag.value}")
        println(s"anotherFlag = ${anotherFlag.value}")
    }
}