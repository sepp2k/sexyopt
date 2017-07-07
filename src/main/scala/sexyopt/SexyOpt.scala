package sexyopt

import scala.collection.mutable

trait SexyOpt {
    private sealed abstract class NamedArg {
        val longName: String
        val shortName: Option[Char]
        val description: String

        def optString = shortName match {
            case Some(s) => s"-$s, --$longName"
            case None => s"    --$longName"
        }
    }
    private case class Flag(longName: String, shortName: Option[Char], description: String, callback: () => Unit) extends NamedArg
    private case class StringOption(longName: String, shortName: Option[Char], description: String, callback: String => Unit) extends NamedArg

    sealed abstract class Count(val suffix: String, val mustBeLast: Boolean, val infinite: Boolean)
    case object ZeroOrOne extends Count("?", mustBeLast = true, infinite = false)
    case object One extends Count("", mustBeLast = false, infinite = false)
    case object ZeroOrMore extends Count("*", mustBeLast = true, infinite = true)
    case object OneOrMore extends Count("+", mustBeLast = true, infinite = true)

    private case class Positional(name: String, description: String, count: Count, callback: String => Unit) {
        def mustBeLast = count.mustBeLast
        def argString = name + count.suffix
        def required = count match {
            case One | OneOrMore => true
            case ZeroOrOne | ZeroOrMore => false
        }
    }

    private val helpOption = Flag("help", Some('h'), "Display this help message and exit", () => {
        println(usage)
        sys.exit(0)
    })
    private val longNames = mutable.Map[String, NamedArg]("help" -> helpOption)
    private val shortNames = mutable.Map[Char, NamedArg]('h' -> helpOption)
    private val posArgs = mutable.ArrayBuffer[Positional]()
    private var initialized = false

    class Argument[T](init: T) {
        private var _value = init
        def value = {
            if(!initialized) throw new IllegalStateException("Argument has been accessed before parse method was called")
            _value
        }
        private[SexyOpt] def value_=(newValue: T) = {
            _value = newValue
        }
    }

    private def addNamedArg(arg: NamedArg) = {
        if(longNames.isDefinedAt(arg.longName)) {
            throw new IllegalStateException(s"Option --${arg.longName} has already been defined.")
        }
        longNames(arg.longName) = arg
        arg.shortName.foreach { shortName =>
            if(shortNames.isDefinedAt(shortName)) {
                throw new IllegalStateException(s"Option -$shortName has already been defined.")
            }
            shortNames(shortName) = arg
        }
    }

    private def flag(longName: String, shortName: Option[Char], description: String): Argument[Boolean]  = {
        val arg = new Argument(false)
        addNamedArg(Flag(longName, shortName, description, () => arg.value = true))
        arg
    }

    def flag(longName: String, shortName: Char, description: String): Argument[Boolean]  = {
        flag(longName, Some(shortName), description)
    }

    def flag(longName: String, description: String): Argument[Boolean]  = {
        flag(longName, None, description)
    }

    private def option(longName: String, shortName: Option[Char], description: String, default: String): Argument[String]  = {
        val arg = new Argument[String](default)
        addNamedArg(StringOption(longName, shortName, description, s => arg.value = s))
        arg
    }

    private def option(longName: String, shortName: Option[Char], description: String): Argument[Option[String]]  = {
        val arg = new Argument[Option[String]](None)
        addNamedArg(StringOption(longName, shortName, description, s => arg.value = Some(s)))
        arg
    }

    def option(longName: String, shortName: Char, description: String, default: String): Argument[String]  = {
        option(longName, Some(shortName), description, default)
    }

    def option(longName: String, shortName: Char, description: String): Argument[Option[String]]  = {
        option(longName, Some(shortName), description)
    }

    def option(longName: String, description: String, default: String): Argument[String]  = {
        option(longName, None, description, default)
    }

    def option(longName: String, description: String): Argument[Option[String]]  = {
        option(longName, None, description)
    }

    private def addPosArg(arg: Positional) = {
        if(posArgs.nonEmpty && posArgs.last.mustBeLast) {
            throw(new IllegalStateException("There can't be more than one optional or variadic positional argument"))
        }
        posArgs += Positional(arg.name, arg.description, arg.count, arg.callback)
    }

    def posArg(name: String, description: String): Argument[String]  = {
        val arg = new Argument[String]("")
        addPosArg(Positional(name, description, One, s => arg.value = s))
        arg
    }

    def optionalPosArg(name: String, description: String, default: String): Argument[String]  = {
        val arg = new Argument[String](default)
        addPosArg(Positional(name, description, ZeroOrOne, s => arg.value = s))
        arg
    }

    def optionalPosArg(name: String, description: String): Argument[Option[String]]  = {
        val arg = new Argument[Option[String]](None)
        addPosArg(Positional(name, description, ZeroOrOne, s => arg.value = Some(s)))
        arg
    }

    def restArgs(name: String, description: String, atLeastOne: Boolean): Argument[Seq[String]] = {
        val args = mutable.ArrayBuffer[String]()
        val count = if (atLeastOne) OneOrMore else ZeroOrMore
        addPosArg(Positional(name, description, count, s => args += s))
        new Argument(args)
    }

    val optionIndentation = 2
    val optionDescriptionOffset = 20
    val totalLength = 80
    final val optionLength = optionDescriptionOffset - optionIndentation
    final val optionDescriptionLength = totalLength - optionDescriptionOffset

    def programName: String
    def programDescription: String

    def usage: String = {
        val argsString = posArgs.map(" " + _.argString).mkString
        val options =
            posArgs.map(arg => (arg.name, arg.description)) ++
                Seq("    --" -> "Treat all subsequent arguments as positional even if they start with a dash") ++
                longNames.values.map(arg => (arg.optString, arg.description))
        // TODO: wrap around
        val optionDescriptions = options.map {
            case (optString, description) =>
                if (optString.length < optionLength) {
                    " " * optionIndentation + optString + " " * (optionLength - optString.length) + description
                } else {
                    " " * optionIndentation + optString + "\n" + " " * optionDescriptionOffset + description
                }
        }.mkString("\n")
        s"Usage: $programName OPTION*$argsString\n$programDescription\n\nOptions:\n$optionDescriptions"
    }

    def parse(args: Array[String]): Unit = {
        if (initialized) throw new IllegalStateException("More than one call to parse method")
        initialized = true
        val remainingArgs = mutable.Queue(args: _*)
        val remainingPosArgs = mutable.Queue(posArgs: _*)
        var ignoreDashes = false
        var infinitePosArgAssignedAtLeastOnce = false
        while (remainingArgs.nonEmpty) {
            val arg = remainingArgs.dequeue
            if (arg == "--" && !ignoreDashes) {
                ignoreDashes = true
            } else if (arg.startsWith("--") && !ignoreDashes) {
                longNames.get(arg.substring(2)) match {
                    case Some(flag: Flag) =>
                        flag.callback()
                    case Some(opt: StringOption) =>
                        if (remainingArgs.isEmpty) failWith(s"Option --${opt.longName} requires an argument.")
                        else opt.callback(remainingArgs.dequeue())
                    case None =>
                        failWith(s"Unknown option $arg")
                }
            } else if (arg.startsWith("-") && !ignoreDashes) {
                arg.substring(1).toCharArray.foreach { c =>
                    shortNames.get(c) match {
                        case Some(flag: Flag) =>
                            flag.callback()
                        case Some(opt: StringOption) =>
                            if (remainingArgs.isEmpty) failWith(s"Option -${opt.shortName} requires an argument.")
                            else opt.callback(remainingArgs.dequeue())
                        case None =>
                            failWith(s"Unknown short option -$c")
                    }
                }
            } else {
                if (remainingPosArgs.isEmpty) {
                    failWith("Too many arguments.")
                }
                val posArg = remainingPosArgs(0)
                posArg.callback(arg)
                if(posArg.count.infinite) {
                    infinitePosArgAssignedAtLeastOnce = true
                } else {
                    remainingPosArgs.dequeue()
                }
            }
        }
        if (remainingPosArgs.nonEmpty && remainingPosArgs(0).required && !infinitePosArgAssignedAtLeastOnce) {
            failWith(s"Missing value for ${remainingPosArgs(0).name}.")
        }
    }

    def failWith(message: String): Nothing = {
        System.err.println(message)
        System.err.println(s"See $programName --help for more information")
        sys.exit(1)
    }
}