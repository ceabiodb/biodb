#' @details
#' The first step in using *biodb*, is to create an instance of the
#' main class `Biodb`. This is done by calling the constructor of
#' the class.
#'
#' The OOP (Object Oriented Programming) system used in the *biodb*
#' package is the **RC** system also called **R5**. In this system,
#' the created objects are not copied, but their reference are
#' copied.  This means that when you pass an instance to a
#' function, that function is able to modify the instance.  Also,
#' in R5, methods are attached to the object. The calling mechanism
#' is thus slightly different: `myObject$myFunction()` instead of
#' `myFunction(myObject)` in S4.
#'
#' To the Biodb instance are attached several singleton class
#' instances: BiodbConfig, BiodbFactory, BiodbPersistentCache,
#' BiodbDbsInfo and BiodbEntryFields. Each of these classes gives
#' access to a set of functionalities. BiodbFactory is responsible
#' for creating the database connectors. BiodbConfig allows reading
#' and writing of configuration values. BiodbPersistentCache gives
#' you access to the cache system on disk. BiodbDbsInfo enables to
#' access information about databases. BiodbEntryFields gives you
#' information about the definition of the entry fields that are
#' used to store data parsed from entry contents.
#'
#' @examples
#' # Create an instance of the Biodb class:
#' mybiodb <- biodb::Biodb()
#'
#' # Getting the configuration instance:
#' cfg <- mybiodb$getConfig()
#'
#' # Stop messages of the Biodb instance:
#' cfg$set('msg.info.lvl', 0)
#'
#' # Increase verbosity level of the Biodb instance:
#' cfg$set('msg.info.lvl', 2)
#' cfg$set('msg.debug.lvl', 2)
#' cfg$set('msg.caution.lvl', 2)
#'
#' Do not forget to terminate your biodb instance once you are done with it:
#' mybiodb$terminate()
#'
#' @seealso \link{Biodb}, \link{BiodbConfig}, \link{BiodbFactory}, \link{BiodbPersistentCache},
#' \link{BiodbDbsInfo}, \link{BiodbEntryFields}.
#'
"_PACKAGE"
#' @useDynLib biodb
NULL
