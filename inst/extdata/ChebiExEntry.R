ChebiExEntry <- R6::R6Class("ChebiExEntry",
inherit=BiodbXmlEntry,

public=list(

initialize=function(...) {
    super$initialize(...)
}
),

private=list(
doCheck=function(content) {
    
    # You can do some more checks of the content here.
    
    return(TRUE)
}

,doParseFieldsStep2=function(parsed.content) {
    
    # TODO Implement your custom parsing processing here.
}

))
