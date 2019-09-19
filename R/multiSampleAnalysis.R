

#' Make a nice plot from cellularities of sequenza
#'
#' @param ... allows arbitrary amounts of ci objects
#' @param inList ci objects can also be specified in a list object
#' @param col the colors to be used for plotting
#' @param labels the names of the samples to be used for plotting
#'
#' @return nothing
#' @export
#'
#' @examples
#' #coming soon to your town
plotSequenzaCellularity <- function(...,inList=NULL,col,labels){

  #if ther is no list to iterate through, we iterate through ...
  obs <- c()
  if(is.null(inList)){
    dotArgs <- list(...)
    #get only the dotargs which are unnamed
    dotNames <- names(dotArgs)
    if ( length(dotNames) == 0 ){
      #nothing else but unnamed inputs
      obs <- dotArgs
      dotArgs <- NULL

    }else{
      get <- dotNames==""
      obs <- dotArgs[get]

      # remove dotargs used
      dotArgs <- dotArgs[!get]

    }

  }else{
    obs <- inList
  }

  #default colouring if nothing is set
  if(missing(col)){
    col <- grDevices::rainbow(length(obs))
  }

  #number the samples if no names are set
  if(missing(labels)){
    labels <- c(1:length(obs))
  }

  cels <- vector(mode = "numeric",length = length(obs))
  upperCI <- vector(mode = "numeric", length = length(obs))
  lowerCI <- vector(mode = "numeric", length = length(obs))

  for (i in 1:length(obs)) {
      ciObj <- obs[[i]]
      # get estimated cellularity value
      cels[i] <- ciObj$max.cellularity
      # get confidence intervals
      lowerCI[i] <- ciObj$confint.cellularity[1]
      upperCI[i] <- ciObj$confint.cellularity[2]

  }

  #create the plot region and the average line
  graphics::plot.default(y=cels,x=c(1:length(cels)), ylab="Estimated cellularity", xaxt='n', ylim=c(0,1), xlim=c(0.7,length(cels)), col=col, bty='n', xlab="", panel.first={graphics::abline(h=mean(cels,na.rm=TRUE),col="grey",lty=2)},type='p',pch=15,las=2,cex=2)

  #add the ticks and set the orientation of the axis
  graphics::axis(1,at=c(1:length(cels)),labels=NA,las=3)

  #add the labels of the samples
  graphics::text(x=c(1:length(cels)), y=-0.1, labels=labels, srt=45, pos=1,xpd=T)

  #add the confidence intervals for each of the estimates
  for (i in 1:length(upperCI)){
    graphics::segments(i, lowerCI[i], i, upperCI[i], col=col[i])
  }

}
