rm(list=ls())
init <- Sys.time(); timer <- proc.time();
#-----------------------------------------------------
# Combine a bunch of json data dictionaries
# into a single file
#-----------------------------------------------------


#-----------------------------------------------------
# Load required packages
#-----------------------------------------------------
require(getopt)
require(jsonlite)

#-----------------------------------------------------
# Required Functions
#-----------------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))

add_to_log <- function(lvl, func, message, add=T, logger = file.path(args$outpath, args$outfile)){
  # <Date> <function> <level> <information>
  timestamp <- paste0("[",Sys.time(),"]")
  entry <- paste(timestamp, func, toupper(lvl), message, sep = " - ")
  
  
  if (!exists("logbook")){
    # This is the first time that logging is being called
    # Fresh version of the logfile desired???
    cat(paste(c(rep("-",80), "\n"), collapse = ""), file = logger, append = F)
    cat(paste0(c(rep(" ",27), "CDE Harmonization Validator", "\n"), collapse = ""), file = logger, append = T)
    cat(paste(c(rep("-",80), "\n"), collapse = ""), file = logger, append = T)
    logbook <<-  setNames(data.frame(matrix(ncol = 4, nrow = 0), stringsAsFactors = F), c("timestamp", "lvl", "func", "message"))
  }
  
  
  if (add){
    cat(paste0(entry, "\n"), file = logger, append = T)
    logbook <<- rbind(logbook, data.frame("timestamp" = timestamp,"lvl" = toupper(lvl), "func"=func, "message"=message, stringsAsFactors = F))
  }
  
  message(paste0(entry, "\n"))
}

read_dd <- function(x){
  dd <- fromJSON(x, flatten = T) # file.path(fpath, f)
  #dd <- fromJSON(dd)
  #dd <- as.data.frame(dd)
  return(dd)
}

#-----------------------------------------------------
# Setup global arguments
#-----------------------------------------------------
argString <- commandArgs(trailingOnly = T)

usage <- paste("Usage: Rscript convert_dd_to_json.R
               -- Required Parameters --
               [-i | --inpath]           <Path to the .xslx data dictionary>
               [-n | --prefix]           <File Prefix>
               -- Optional Parameters -- 
               [-o | --outfile]          <The filename for the output> (default = name of the -infile with a _<Sheet Name>.json suffix)
               [-p | --outpath]          <Path to the output directory> (default = the same directory as the --infile)
               -- Optional Flags --   
               [-v | --verbose]          <Verbose output> (default = F)
               -- Help Flag --  
               [-h | --help]             <Displays this help message>
               Example:
               Rscript combine_dd.R -i <path> 
               \n",sep="")

#0=no-arg, 1=required-arg, 2=optional-arg
spec <- matrix(c(	
  'inpath',  'i', 1, "character",
  'outfile', 'o', 2, "character",
  'outpath', 'p', 2, "character",
  'prefix',  'n', 1, "character",
  'verbose', 'v', 0, "logical",
  'help',    'h', 0, "logical"
), byrow=TRUE, ncol=4);

if (length(argString) == 0){
  argString <- c( "-i", "C:/Users/jstratford/OneDrive - Research Triangle Institute/Documents/Projects/CONNECTS/20210901_convert_dd_to_json/2021-11-05/", "-v", "-n" , "CONNECTS_DD_V1.21_clean")
  #argString <- c( "-i", "C:/Users/jstratford/OneDrive - Research Triangle Institute/Documents/Projects/CONNECTS/20210901_convert_dd_to_json/UnitTests", "-v", "-n" , "MasterFile")
}

args=getopt(spec, argString)

if ( !is.null(args$help) | is.null(args$inpath) | is.null(args$prefix) ) {
  add_to_log(lvl="error", func="getopt", message = "Either you asked for help or you are missing a required parameters: infile, domain, SDTM")
  add_to_log(lvl="error", func="getopt", message = usage, add = F)
  q(save="no",status=1,runLast=FALSE)
}

if(is.null(args$unittest)){args$unittest <- F}
if(is.null(args$verbose)){args$verbose <- F}

args$datestamp <- format(Sys.Date(), format="%Y-%m-%d")
if(is.null(args$outpath)){
  if (args$inpath == "."){
    args$outpath <- getwd()
  } else {
    args$outpath <- args$inpath
  }
}

if(is.null(args$outfile)){args$outfile <- paste(args$datestamp, paste0(args$prefix, ".json"), sep="_")}
args$logfile <- file.path(args$outpath, paste0(args$prefix, "_concatenate.log"))

#-----------------------------------------------------
#                 Method Execution                   #
#-----------------------------------------------------

#-----------------------------------------------------
# Logging information
#-----------------------------------------------------
add_to_log(lvl = "info", func="main", message=paste0("User: ", Sys.info()[['effective_user']]) )
add_to_log(lvl = "info", func="main", message=paste0("Running from: ", Sys.info()[['nodename']]) )
add_to_log(lvl = "info", func="main", message=paste0("Platform: ", sessionInfo()$platform) )
add_to_log(lvl = "info", func="main", message=paste0("R version: ", sessionInfo()$R.version$version.string) )
add_to_log(lvl = "info", func="main", message=paste0("R packages loaded: ",  paste(names(sessionInfo()$otherPkgs), collapse=", ") ) )
add_to_log(lvl = "info", func="main", message=paste0("Rscript: ", gsub("--file=", "", grep(pattern = "^--file", commandArgs(trailingOnly = F), value = T))))
add_to_log(lvl = "info", func="main", message=paste0("Arguments: ", paste(commandArgs(trailingOnly = T), collapse=" ")) )
add_to_log(lvl = "info", func="getopt", message=paste0("Params: ", paste(names(args), args, sep=" = ")))

#-----------------------------------------------------
# Read in the data dictionaries
#-----------------------------------------------------
files <- list.files(path = args$inpath, pattern = paste0("^",args$prefix, ".*json$"), full.names = T)

dds <- list()
for ( f in files) {
  domain <- strsplit( x = basename(gsub(pattern = ".json$", "", f, perl = T)), split = "clean_")[[1]][2]
  add_to_log(lvl="info", func = "read_dd", message = paste0("Reading in data dictionary for domain ", domain))
  dds[toupper(domain)] <- read_dd(f)
}

out <- jsonlite::toJSON(dds, pretty = F)
write_json(x=out, path=file.path(args$outpath, args$outfile))

add_to_log(lvl = "info", func="main", message=paste0("Process began at ", init, " and finished at ", Sys.time()), add = F)
add_to_log(lvl = "info", func="main", message=paste0("Elapsed time: ", (proc.time() - timer)[['elapsed']]), add = F)
add_to_log(lvl = "info", func="main", message="Finished", add = F)
