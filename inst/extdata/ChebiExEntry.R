ChebiExEntry <- R6::R6Class("ChebiExEntry",
inherit=BiodbXmlEntry,

public=list(

initialize=function(...) {
    super$initialize(...)
}
),

private=list(
isContentCorrect=function(content) {
 
    correct <- super$isContentCorrect(content)
    
    # You can do some more checks of the content here.
    
    return(correct)
}

,parseFieldsStep2=function(parsed.content) {
    
    # TODO Implement your custom parsing processing here.
}

))
