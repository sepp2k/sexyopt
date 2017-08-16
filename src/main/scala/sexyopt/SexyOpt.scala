package sexyopt

import scala.language.implicitConversions
import scala.collection.mutable

/**
 * Mix-in this trait to define the command line options and arguments that your application takes.
 */
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

    private sealed abstract class Count(val suffix: String, val mustBeLast: Boolean, val infinite: Boolean)
    private case object ZeroOrOne extends Count("?", mustBeLast = true, infinite = false)
    private case object One extends Count("", mustBeLast = false, infinite = false)
    private case object ZeroOrMore extends Count("*", mustBeLast = true, infinite = true)
    private case object OneOrMore extends Count("+", mustBeLast = true, infinite = true)

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

    /**
     * A value provided by the user as a command line option or argument. Do not use until `parse` has been called.
     */
    class Argument[T](init: T) {
        private var _value = init
        /**
         * The value provided by the user. Do not use until `parse` has been called.
         */
        def value = {
            if(!initialized) throw new IllegalStateException("Argument has been accessed before parse method was called")
            _value
        }
        private[SexyOpt] def value_=(newValue: T) = {
            _value = newValue
        }

        override def toString = value.toString
    }

    implicit def getArgumentValue[T](arg: Argument[T]): T = arg.value

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

    /**
     * Adds a flag (option without an argument).
     *
     * @param longName the long name of the flag. The flag can be activated via `--longName`
     * @param shortName the single-character name of the flag. The flag can be activated via `-shortName`
     * @param description the description that will be displayed for this flag in the `--help` message
     * @return `true` if the flag has been used and `false` otherwise
     */
    def flag(longName: String, shortName: Char, description: String): Argument[Boolean]  = {
        flag(longName, Some(shortName), description)
    }

    /**
     * Adds a flag (option without an argument).
     *
     * @param longName the long name of the flag. The flag can be activated via `--longName`
     * @param description the description that will be displayed for this flag in the `--help` message
     * @return `true` if the flag has been used and `false` otherwise
     */
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

    /**
     * Adds an option with an argument. The option's argument will be the token that follows the option on the command line.
     *
     * @param longName the long name of the option. The option can be activated via `--longName argument`
     * @param shortName the single-character name of the option. The option can be activated via `-shortName argument`
     * @param description the description that will be displayed for this option in the `--help` message
     * @param default the value that is used when the user does not use this option
     * @return the argument provided by the user or `default` if the user did not use this option
     */
    def option(longName: String, shortName: Char, description: String, default: String): Argument[String]  = {
        option(longName, Some(shortName), description, default)
    }

    /**
     * Adds an option with an argument. The option's argument will be the token that follows the option on the command line.
     *
     * @param longName the long name of the option. The option can be activated via `--longName argument`
     * @param shortName the single-character name of the option. The option can be activated via `-shortName argument`
     * @param description the description that will be displayed for this option in the `--help` message
     * @return the argument provided by the user wrapped in a `Some` or `None` if the user did not use this option
     */
    def option(longName: String, shortName: Char, description: String): Argument[Option[String]]  = {
        option(longName, Some(shortName), description)
    }

    /**
     * Adds an option with an argument. The option's argument will be the token that follows the option on the command line.
     *
     * @param longName the long name of the option. The option can be activated via `--longName argument`
     * @param description the description that will be displayed for this option in the `--help` message
     * @param default the value that is used when the user does not use this option
     * @return the argument provided by the user or `default` if the user did not use this option
     */
    def option(longName: String, description: String, default: String): Argument[String]  = {
        option(longName, None, description, default)
    }

    /**
     * Adds an option with an argument. The option's argument will be the token that follows the option on the command line.
     *
     * @param longName the long name of the option. The option can be activated via `--longName argument`
     * @param description the description that will be displayed for this option in the `--help` message
     * @return the argument provided by the user wrapped in a `Some` or `None` if the user did not use this option
     */
    def option(longName: String, description: String): Argument[Option[String]]  = {
        option(longName, None, description)
    }

    private def addPosArg(arg: Positional) = {
        if(posArgs.nonEmpty && posArgs.last.mustBeLast) {
            throw(new IllegalStateException("There can't be more than one optional or variadic positional argument"))
        }
        posArgs += Positional(arg.name, arg.description, arg.count, arg.callback)
    }

    /**
     * Adds a required positional argument. The order of positional arguments will be the order in which this
     * method is called.
     *
     * @param name the name of this argument as it appears in the `--help` message
     * @param description the description that will be displayed for this argument in the `--help` message
     * @return the argument provided by the user
     */
    def posArg(name: String, description: String): Argument[String]  = {
        val arg = new Argument[String]("")
        addPosArg(Positional(name, description, One, s => arg.value = s))
        arg
    }

    /**
     * Adds an optional positional argument. The order of positional arguments will be the order in which this
     * method is called. No other positional argument may be added after this one.
     *
     * @param name the name of this argument as it appears in the `--help` message
     * @param description the description that will be displayed for this argument in the `--help` message
     * @param default the value that is used when the user does not use this option
     * @return the argument provided by the user if there is one or `default` otherwise
     */
    def optionalPosArg(name: String, description: String, default: String): Argument[String]  = {
        val arg = new Argument[String](default)
        addPosArg(Positional(name, description, ZeroOrOne, s => arg.value = s))
        arg
    }

    /**
     * Adds an optional positional argument. The order of positional arguments will be the order in which this
     * method is called. No other positional argument may be added after this one.
     *
     * @param name the name of this argument as it appears in the `--help` message
     * @param description the description that will be displayed for this argument in the `--help` message
     * @return the argument provided by the user wrapped in a `Some` if there is one or `None` otherwise
     */
    def optionalPosArg(name: String, description: String): Argument[Option[String]]  = {
        val arg = new Argument[Option[String]](None)
        addPosArg(Positional(name, description, ZeroOrOne, s => arg.value = Some(s)))
        arg
    }

    /**
     * Eats up all remaining positional arguments. No other positional argument may be added after this one.
     *
     * @param name the name of this argument as it appears in the `--help` message
     * @param description the description that will be displayed for this argument in the `--help` message
     * @param atLeastOne `false` if no arguments may be provided, `true` if at least one has to be provided
     * @return the argument provided by the user if there is one or `default` otherwise
     */
    def restArgs(name: String, description: String, atLeastOne: Boolean): Argument[Seq[String]] = {
        val args = mutable.ArrayBuffer[String]()
        val count = if (atLeastOne) OneOrMore else ZeroOrMore
        addPosArg(Positional(name, description, count, s => args += s))
        new Argument(args)
    }

    /**
     * Override this to change the indentation of the options in the `--help` message
     */
    val optionIndentation = 2

    /**
     * Override this to change the start of the second column in the `--help` message
     */
    val optionDescriptionOffset = 20

    /**
     * Override this to change the width at which the `--help` message is wrapped (not currently implemented)
     */
    val totalLength = 80

    private final val optionLength = optionDescriptionOffset - optionIndentation
    private final val optionDescriptionLength = totalLength - optionDescriptionOffset

    /**
     * The name of this program as displayed in the usage section of the `--help` message
     */
    def programName: String

    /**
     * The description of this program that is displayed in the `--help` message
     */
    def programDescription: String

    private def usage: String = {
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

    /**
     * Parses the given list of command line arguments and sets the defined arguments accordingly.
     */
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

    /**
     * Override this to change what happens when the given command line is not valid (for example because
     * arguments are missing or there are too many).
     *
     * By default this displays the given message (plus a reminder to use `--help` for more information) to
     * stderr and exits the process with exit code 1.
     */
    def failWith(message: String): Nothing = {
        System.err.println(message)
        System.err.println(s"See $programName --help for more information")
        sys.exit(1)
    }
}