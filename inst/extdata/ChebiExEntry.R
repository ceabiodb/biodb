ChebiExEntry <- methods::setRefClass("ChebiExEntry",
    contains="BiodbXmlEntry",

methods=list(

initialize=function(...) {
    callSuper(...)
}

,.isContentCorrect=function(content) {
 
    correct <- callSuper(content)
    
    # You can do some more checks of the content here.
    
    return(correct)
}

,.parseFieldsStep2=function(parsed.content) {
    
    # TODO Implement your custom parsing processing here.
}

))
