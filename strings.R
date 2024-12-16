###############################################################################
# BipartGraph
#  
# Module         : strings.R
# Description    : User interfaz
###############################################################################

LocalizedStrings<-function(locale="es") {
  # crea el objeto
  this<-list(locale=locale, data=read.csv("resources/strings.csv", header=TRUE, row.names=1,  colClasses="character", encoding="ISO-8859-1"))
  
  # obtiene el texto para la clave indicada
  this$value<-function(keys) {
    trim <- function (x) {
      gsub("^\\s+|\\s+$", "", iconv(x, from = "ISO-8859-1", to = "UTF-8"))
    }
    val<-this$data[keys, this$locale]
    val[is.na(val)]<-"(error: undefined key)"
    val[is.null(val)]<-"(error: undefined language)"
    return(trim(val))
  }
  
  return(this)
}