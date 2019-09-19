
#turn on data.table
.datatable.aware = TRUE

#allow the use of those variables for data.table
if (getRversion() >= "2.15.1")
  utils::globalVariables(c(".", ".I", ".N", ".SD"), utils::packageName())

#' Reads structural variants vcf
#'
#' Turns the vcf into an annotated data.table
#'
#' @param file the vcf file to be read in
#' @param minlength minimum length of a variant to be considered
#' @param chrs which chromosomes to restrict the variants on
#' @param gns which genes to use to annotate
#' @param biomaRtDB which biomart db to use
#'
#' @import TxDb.Hsapiens.UCSC.hg19.knownGene
#' @import org.Hs.eg.db
#' @importFrom data.table ":="
#'
#' @return data.table with annotated structural variants
#' @export
#'
#' @examples
#' #minimal example here
getStructuralVariants <- function(file, minlength=10000,chrs=c(1:22, "X", "Y"),gns,biomaRtDB) {

  sv_vcf <-VariantAnnotation::readVcf(file,"hg19")

  # get the genes in the ranges
  sv_gr <- StructuralVariantAnnotation::breakpointRanges(sv_vcf)

  #remove small variants on the same chromosome
  sv_gr <- sv_gr[GenomeInfoDb::seqnames(sv_gr) != GenomeInfoDb::seqnames(StructuralVariantAnnotation::partner(sv_gr)) | abs(BiocGenerics::start(sv_gr) - BiocGenerics::start(StructuralVariantAnnotation::partner(sv_gr))) > minlength]

  #for biomarot we need the chr in the front
  GenomeInfoDb::seqlevelsStyle(sv_gr) <- "UCSC"

  #check if the supplied chrs already have UCSC style, otherwise add the chr
  if( ! all(grepl("^chr",chrs))){
    chrs<- paste0("chr",chrs)
  }

  # remove non canonical variants
  sv_gr_can <- sv_gr[GenomeInfoDb::seqnames(sv_gr) %in% chrs]
  # now remove entries that had partners there
  sv_gr_can <- sv_gr_can[sv_gr_can$partner %in% names(sv_gr_can)]

  GenomeInfoDb::seqlevels(sv_gr_can) <- chrs

  # if there were no genes supplied, we get our own
  if (missing(gns)){
    gns <- GenomicFeatures::genes(TxDb.Hsapiens.UCSC.hg19.knownGene)
  }else{
    # do some test to see if the arugment makes sense
    if( class(gns)[1] != "GRanges"){
      warning("This does not look like a proper biomaRtDB")
      #do we exit here or wait for biomaRt to throw the error?
    }
  }
  # same goes for the database
  if(missing(biomaRtDB)){
    biomaRtDB <- org.Hs.eg.db
  }else{
    # do some test to see if the arugment makes sense
    if( class(biomaRtDB)[1] != "OrgDb"){
      warning("This does not look like a proper biomaRtDB")
      #do we exit here or wait for biomaRt to throw the error?
    }
  }

  hits <- data.table::as.data.table(GenomicRanges::findOverlaps(sv_gr_can, gns, ignore.strand=TRUE))
  hits$SYMBOL <- biomaRt::select(biomaRtDB, gns[hits$subjectHits]$gene_id, "SYMBOL")$SYMBOL
  hits$gene_strand <- as.character(BiocGenerics::strand(gns[hits$subjectHits]))

  #we use this so R CMD does not complain about unset variables
  SYMBOL <- NULL
  queryHits <- NULL
  #now we append all symbols into one line
  hits[,SYMBOL:=paste(.SD$SYMBOL,collapse = ","), by=list(queryHits)]

  sv_gr_can$SYMBOL <- NA
  sv_gr_can$SYMBOL[hits$queryHits] <- hits$SYMBOL
  sv_gr_can$geneStrand <- NA
  sv_gr_can$geneStrand[hits$queryHits] <- hits$gene_strand

  #annotate the frequency of the variant
  bafs <-apply(VariantAnnotation::geno(sv_vcf[sv_gr_can$sourceId])$PR,1,function(x) {
    normal <- x[[1]]
    tumor <- x[[2]]

    baf1Normal <- normal[1]/sum(normal,na.rm=T)
    baf2Normal <- normal[2]/sum(normal,na.rm=T)

    baf1Tumor <- tumor[1]/sum(tumor,na.rm=T)
    baf2Tumor <- tumor[2]/sum(tumor,na.rm=T)

    return(c(baf1Normal,baf2Normal,baf1Tumor,baf2Tumor))
  })

  #flip them in the right way and add names
  bafs <- as.data.frame(t(bafs))
  colnames(bafs) <- c("mafNormal","bafNormal","mafTumor","bafTumor")

  # we do this the ugly way, because otherwise it wont stay a granges object
  sv_gr_can$mafNormal <- bafs$mafNormal
  sv_gr_can$bafNormal <- bafs$bafNormal
  sv_gr_can$mafTumor <- bafs$mafTumor
  sv_gr_can$bafTumor <- bafs$bafTumor

  #depth of coverage is sometimes a nice info, even though for duplications and similar it is not really a thing
  sv_gr_can$depth <- VariantAnnotation::info(sv_vcf[sv_gr_can$sourceId])$BND_DEPTH

  # need to convert to dataframe first to keep the rownames
  sv_dt <- data.table::as.data.table(as.data.frame(sv_gr_can),keep.rownames= "ID")

  #self join on the partner to get the info of both break points
  sv_dt_all <- merge(sv_dt,sv_dt,by.x="ID",by.y="partner")

  # now we remove the duplications
  mn <- pmin(sv_dt_all$partner, sv_dt_all$ID)
  mx <- pmax(sv_dt_all$partner, sv_dt_all$ID)
  int <- as.numeric(interaction(mn, mx))
  sv_dt_nodup<- sv_dt_all[match(unique(int), int),]

  #set the strand to a usuable format
  sv_dt_nodup$geneStrand.x <- as.character(sv_dt_nodup$geneStrand.x)
  sv_dt_nodup$geneStrand.x[is.na(sv_dt_nodup$geneStrand.x)] <- ""
  sv_dt_nodup$geneStrand.y <- as.character(sv_dt_nodup$geneStrand.y)
  sv_dt_nodup$geneStrand.y[is.na(sv_dt_nodup$geneStrand.y)] <- ""

  #and do the same with the gene
  sv_dt_nodup$SYMBOL.x[is.na(sv_dt_nodup$SYMBOL.x)] <- ""
  sv_dt_nodup$SYMBOL.y[is.na(sv_dt_nodup$SYMBOL.y)] <- ""
  return(sv_dt_nodup)
}
