#' @details
#'
#' # Initialization
#'
#' The first step in using *biodb*, is to create an instance of the
#' main class `Biodb`. This is done by calling the constructor of
#' the class:
#' ```
#' mybiodb <- biodb::Biodb()
#' ```
#'
#' # OOP mechanism
#'
#' The OOP (Object Oriented Programming) system used in the *biodb*
#' package is the **RC** system also called **R5**. In this system,
#' the created objects are not copied, but their reference are
#' copied. This means that when you pass an instance to a
#' function, that function is able to modify the instance.  Also,
#' in R5, methods are attached to the object. The calling mechanism
#' is thus slightly different, in **RC** we write `myObject$myFunction()`
#' instead of `myFunction(myObject)` in S4. See
#' <https://www.rdocumentation.org/packages/methods/versions/3.6.0/topics/ReferenceClasses>
#' for more details.
#'
#' # Singleton classes
#'
#' To the Biodb instance are attached several singleton class instances. Each
#' of them gives access to a different set of *biodb* features:
#' * `BiodbFactory` is responsible for creating the database connectors.
#' * `BiodbConfig` gives access to configuration values.
#' * `BiodbPersistentCache` handles the cache system on disk.
#' * `BiodbDbsInfo` registers information about databases.
#' * `BiodbEntryFields` lists information about the definition of the entry
#' fields that are used to store data parsed from entry contents.
#'
#' To access those singleton instances you must use the following methods:
#' ```
#' mybiodb$getFactory()
#' mybiodb$getConfig()
#' mybiodb$getPersistentCache()
#' mybiodb$getDbsInfo()
#' mybiodb$getEntryFields()
#' ```
#'
#' # Available database connectors
#'
#' This package is delivered with two connectors for local databasses:
#' MassCsvFile annd MassSqlite. However it is extendable, and in fact other
#' packages already exist or will soon be made available on Bioconductor or
#' GitHub for accessing other databases like ChEBI, Uniprot, HMDB, KEGG,
#' Massbank or Lipidmaps.  You may also write your own connector by extending
#' *biodb*. If you are interested, a vignette explains what you need to do in
#' details.
#'
#' When creating the instance of the `Biodb` class you should have received a
#' message like "Loading definitions from package ..." if any extending package
#' has also been installed on your system. Connector definitions found in
#' extending packages are automatically loaded when instantiating `Biodb`, thus
#' you do not need to call `library()` to individually load each extending
#' package.
#'
#' To get a list of available connectors, simply print information about your
#' `Biodb` instance:
#' ```
#' mybiodb
#' ```
#'
#' # Getting a database connector
#'
#' To access a database you need a connector that you obtain from
#' the factory instance. Here is an example with a compound CSV file database:
#' ```
#' chebi.tsv <- system.file("extdata", "chebi_extract.tsv", package='biodb')
#' conn <- mybiodb$getFactory()$createConn('comp.csv.file', url=chebi.tsv)
#' ```
#'
#' Then the connector allows you to send requests to the database
#' to retrieve entries directly or run more complex queries:
#' ```
#' conn$getEntry('1018')
#' conn$searchCompound(mass=136.05, mass.field='monoisotopic.mass')
#' ```
#'
#' # Biodb messages
#'
#' *biodb* implements five level of messages:
#' * `error` which is mapped to R error messages.
#' * `warning` which is mapped to R warning messages.
#' * `caution` which uses R message() method.
#' * `info` which uses R message() method.
#' * `debug` which uses R message() method.
#'
#' The three last (`caution`, `info` and `debug`) use each multiple levels.
#' By default debug messages are disabled. To change the level of a message
#' type, you need to access its configuration variable. Here we silence the
#' caution and info messages by setting their maximum level allowed to 0:
#' ```
#' cfg$mybiodb$getConfig()$set('msg.caution.lvl', 0)
#' cfg$mybiodb$getConfig()$set('msg.info.lvl', 0)
#' ```
#'
#' # Termination
#'
#' When you are done working with your *biodb* instance, you must release it by
#' calling the `terminate()` method on it.
#'
#' @examples
#' # Create an instance of the Biodb class:
#' mybiodb <- biodb::Biodb()
#'
#' # Get the databases info instance
#' dbsinfo <- mybiodb$getDbsInfo()
#'
#' # List available connectors
#' dbsinfo$getIds()
#'
#' # Get the factory instance
#' fact <- mybiodb$getFactory()
#'
#' # Get a connector
#' chebi.tsv <- system.file("extdata", "chebi_extract.tsv", package='biodb')
#' conn <- mybiodb$getFactory()$createConn('comp.csv.file', url=chebi.tsv)
#'
#' # Get an entry
#' entry <- conn$getEntry('1018')
#'
#' # Get a list of available fields for this entry
#' entry$getFieldNames()
#'
#' # Get a field value from an entry
#' entry$getFieldValue('accession')
#' entry$getFieldValue('monoisotopic.mass')
#'
#' # Export all field values as a data frame
#' entry$getFieldsAsDataframe()
#'
#' # Get the entry fields instance:
#' fieldDefs <- mybiodb$getEntryFields()
#'
#' # Get information about a field
#' fieldDefs$get('monoisotopic.mass')
#'
#' # Getting the configuration instance:
#' cfg <- mybiodb$getConfig()
#'
#' # Stop information messages of the Biodb instance:
#' cfg$set('msg.info.lvl', 0)
#'
#' # Increase verbosity level of the Biodb instance:
#' cfg$set('msg.info.lvl', 2)
#' cfg$set('msg.debug.lvl', 2)
#' cfg$set('msg.caution.lvl', 2)
#'
#' # Do not forget to terminate your biodb instance once you are done with it:
#' mybiodb$terminate()
#'
#' @seealso \link{Biodb}, \link{BiodbConfig}, \link{BiodbFactory}, \link{BiodbPersistentCache},
#' \link{BiodbDbsInfo}, \link{BiodbEntryFields}.
#'
"_PACKAGE"
#' @useDynLib biodb
NULL
