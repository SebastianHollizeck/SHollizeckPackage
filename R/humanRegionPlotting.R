

#' Change values for plotting
#'
#' @param mappingXML xml representing the picture
#' @param organ which organ to change
#' @param style which style (fill/stroke)
#' @param color which color to plot this organ in
#' @param lwd line width to use for stroke style
#'
#' @return TRUE if something got change FALSE otherwise
#' @export
#'
#' @seealso \code{\link{loadXML}} to get the object, \code{\link{getAllOrgans}}
#'   to find all available sites and \code{\link{drawXML}} to draw the xml object
#'
#' @examples
#' #changes the left lung to be filled with red color
#' mappingXML <- loadXML()
#' changePlottingStyle(mappingXML, "left_lung", color="red")
#' drawXML(mappingXML)
#'
#' #reset page
#' grid::grid.newpage()
#'
#' #changes the left lung to be outlined with blue color
#' mappingXML <- loadXML()
#' changePlottingStyle(mappingXML, "left_lung", color="blue", style='stroke')
#' drawXML(mappingXML)
#'
changePlottingStyle <- function(mappingXML,organ,style = "fill" ,color="black",lwd="5"){

  availableOrgans <- getAllOrgans(mappingXML)

  if( ! organ %in% availableOrgans ){

    warning("Could not find the specified organ: ",organ,", but will continue")
    return(invisible(FALSE))
  }
  parts <- strsplit(organ,"_")[[1]]
  parent <- ""
  if(length(parts) == 3){
    parent <- paste0(parts[c(1,3)],collapse ="_")


  }else if( length(parts) == 2){
    parent <- parts[2]
  }
  if(parent %in% availableOrgans ){
    node<-XML::xpathSApply(mappingXML, paste("//path[@id='",parent,"']",sep=""))[[1]]
    XML::xmlAttrs(node)["type"]<-"stroke"
    node<-XML::xpathSApply(mappingXML, paste("//path[@id='",parent,"']/context/style",sep=""))[[1]]
    XML::xmlAttrs(node)["lwd"]<-1
  }

  # start with the style (either fill or stroke or invis)
  if ( style == "fill" ){
    node<-XML::xpathSApply(mappingXML, paste("//path[@id='",organ,"']",sep=""))[[1]]
    XML::xmlAttrs(node)["type"]<-"fill"
  }else if ( style == "stroke" ){
    #this should be the default, but you never know
    node<-XML::xpathSApply(mappingXML, paste("//path[@id='",organ,"']",sep=""))[[1]]
    XML::xmlAttrs(node)["type"]<-"stroke"
  }else if (style == "invis" ){
    # this is a small workaround, by setting the line width to 0
    node<-XML::xpathSApply(mappingXML, paste("//path[@id='",organ,"']",sep=""))[[1]]
    XML::xmlAttrs(node)["type"]<-"stroke"

    node<-XML::xpathSApply(mappingXML, paste("//path[@id='",organ,"']/context/style",sep=""))[[1]]
    XML::xmlAttrs(node)["lwd"]<-"0"
    return(invisible(TRUE))

  }

  #then go for the color
  node<-XML::xpathSApply(mappingXML, paste("//path[@id='",organ,"']/context/rgb",sep=""))[[1]]
  # dissassemble the color into rgb and then normalize to [0,1]
  rgbCol <- grDevices::col2rgb(color)
  XML::xmlAttrs(node)["r"]=rgbCol[1]/255
  XML::xmlAttrs(node)["g"]=rgbCol[2]/255
  XML::xmlAttrs(node)["b"]=rgbCol[3]/255

  #last we go for the line width, but only if the style is stroke
  node<-XML::xpathSApply(mappingXML, paste("//path[@id='",organ,"']/context/style",sep=""))[[1]]
  if ( style == "stroke" ){
    XML::xmlAttrs(node)["lwd"]<-lwd
  }else{
    XML::xmlAttrs(node)["lwd"]<-1
  }
  return(invisible(TRUE))
}

#' Get all available organs
#'
#' this extracts the top level ids of each child of the root xml
#'
#' @param mappingXML the xml representing the picture
#'
#' @return list of all organs to chose from
#'
#' @export
#'
#' @seealso \code{\link{loadXML}} to get the object, \code{\link{changePlottingStyle}} to change the drawing style and \code{\link{drawXML}} to draw the xml object
#'
#' @examples
#' #get all sites defaultly available
#' getAllOrgans(loadXML())
#'
getAllOrgans <- function(mappingXML){

  root <- XML::xmlRoot(mappingXML)

  children <- XML::xmlChildren(root)

  organs <- sapply(children, function(x) XML::xmlAttrs(x)[2])

  #as the last entry is the one which sums all the others, we do not need the last one
  organs <- organs[-length(organs)]
  return(as.vector(organs))
}


#' Create XML object from file
#'
#' This is mainly used to load the XML representing the human anatomy for plotting purposes
#' can be used for other XMLs as well
#'
#' @param file to be converted into an object
#'
#' @return the XML tree object
#'
#' @export
#'
#' @seealso \code{\link{drawXML}} to draw the XML object and \code{\link{changePlottingStyle}} to change the drawing style of organs
#'
#' @examples
#' #loading the internal xml for plotting
#' mappingXML <- loadXML()
#'
loadXML <- function(file=system.file("extdata", "human_anatomy.ps.xml", package="SHollizeckPackage")){
  #check if the file exists and exit otheriwse
  if(!file.exists(file)){
    stop("Specified file does not exist ", file)
  }

  obj <- XML::xmlParse(file)

  return(obj)
}


#' Plot XMLObject
#'
#' This function converts the XML into a plotable grid object
#'
#' @param mappingXML the XML with the specs to plot
#' @param return where to plot (\code{FALSE}) or return (\code{TRUE}) the object
#'
#' @return the object if return is set to \code{TRUE}
#' @export
#'
#' @seealso \code{\link{loadXML}} to get the XML object and \code{\link{changePlottingStyle}} to change the drawing style of organs
#'
#' @examples
#' #draw the human anatomy
#' drawXML(loadXML())
#'
drawXML <- function(mappingXML, return=FALSE){

  #convert XML to picture object
  pic <- grImport::readPicture(XML::saveXML(mappingXML))
  if(return){
    #return the object instead
    return(pic)
  }else{
    #plot picture
    grImport::grid.picture(pic)
  }

}
