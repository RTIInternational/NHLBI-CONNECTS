rm(list=ls())
init <- Sys.time(); timer <- proc.time();
#-----------------------------------------------------
# This is a script that will convert a data dictionary 
# that was build in Excel into a .json format
# 
# Input: the xlsx sheet 
# Required Columns:
#   IP
#   OP
#   D
#   N
#   Element Description
#   Variable ID
#   Variable Label
#   Type
#   Length
#   Response
#   BDC ID
#   Implementation notes
#
# Output:
#   A json formatted data dictionary
#
# Author: Jeran Stratford
# Date: 01-SEP-2021
#-----------------------------------------------------

#-----------------------------------------------------
# Load required packages
#-----------------------------------------------------
require(openxlsx)
require(getopt)
require(jsonlite)

#-----------------------------------------------------
# Required Functions
#-----------------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))

add_to_log <- function(lvl, func, message, add=T, logger = file.path(args$outpath, args$logfile)){
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

parse_response <- function(x, field="date"){
  x <- gsub(paste0("^\\[",toupper(field),":"), "", x, perl = T)
  x <- gsub(paste0("\\]$"), "", x, perl = T)
  
  if (tolower(field) %in% c("date", "time")){
    # Date format [DATE: <Format> (<description>) ]
    # TIME format [TIME: <Format> (<description>) ]
    out <- strsplit(x, split='\\(', perl = T)[[1]][1]
  } else if (tolower(field) %in% c("derive")){
    # Derive format: [DERIVE: "<response1>" if <condition1> | "<response2]>" if <condition 2> | ...]
    out <- strsplit(x, split = "\\|", perl= T)[[1]]
    out <- sapply(strsplit(out,'"'), `[`, 2)
  } else if (tolower(field) %in% c("from")){
    # PT2 format: [From <sheetname> TAB: <Field Description> (<Column Name>)]
    # CDASH format: [From CDASH controlled terminology for "<Terminology Title>" (<code>) <Optional: for statement> <Optional: Examples>]
    out <- strsplit(x, "\\(", perl = T)[[1]][2]
    out <- strsplit(out, "\\)", perl = T)[[1]][1]
  }
  
  return(trimws(out))
}

read_SDTM <- function(path = args$sdtm, vers = "SDTM Terminology 2020-06-26"){
  tmp <- read.xlsx(xlsxFile=path, sheet=vers, colNames=T, startRow = 1)
  
  # What codelists do I need to retain
  #CDE Variable	  CDASH Code List	  SDTM Code List
  #INTDOSU	      C78423	          C71620
  #INTDOSFRM	    C78426	          C66726
  #INTDOSROUTE	  C78425	          C66729
  #INTDOSFRQ	    C78745	          C71113
  #CMDOSU         C78417
  #CMDOSFRM       C78418
  #CMDOSFRQ       C78419
  #CMROUTE        C78420
  
  out <- tmp[tmp$Codelist.Code %in% c("C71620", "C66726", "C66729", "C71113"), ]
  return(out)
}

read_dd <- function(path = args$infile, domain = args$domain, cdash = sdtm){
  
  tmp <- read.xlsx(xlsxFile=path, sheet=domain, colNames=T, startRow = 4)
  colnames(tmp) <- gsub(pattern = "Response.Options./.Derivation", replacement = "Response.Options", colnames(tmp))
  
  if (paste0(domain, "_pt2") %in% getSheetNames(path)){
    pt2 <- read.xlsx(xlsxFile=args$infile, sheet=paste0(domain, "_pt2"), colNames=T, startRow = 3)
  }
  
  tmp$dataType <- NA
  tmp$dataType[substr(tmp$Response.Options,1,1) != "["] <- "RegulatedChar"
  tmp$dataType[substr(tmp$Response.Options,1,1) == "["] <- "FreeTextChar"
  tmp$dataType[toupper(substr(tmp$Response.Options,1,6)) == "[DATE:"] <- "Date"
  tmp$dataType[toupper(substr(tmp$Response.Options,1,6)) == "[TIME:"] <- "Time"
  tmp$dataType[toupper(substr(tmp$Response.Options,1,6)) == "[FROM "] <- "Reference"
  tmp$dataType[toupper(substr(tmp$Response.Options,1,8)) == "[DERIVE:"] <- "Derived"
  tmp$dataType[toupper(tmp$Variable.Type) == "NUM"] <- "Num"
  
  for (var in tmp$Variable[tmp$dataType == "Reference"]){
    
    response <- tmp[tmp$Variable == var, "Response.Options"]
    ref <- parse_response(response, field="from")
  
    if ( toupper(strsplit(response, " ")[[1]][2]) == "CDASH"){
      
      # Convert from CDASH to SDTM since it is the more extensive list
      term_code <- switch(trimws(ref),
                          # CM DOMAIN
                          "C78417" = "C71620", # units
                          "C78418" = "C66726", # dose form
                          "C78419" = "C71113", # frequency
                          "C78420" = "C66729", # route
                          # INT DOMAIN
                          "C78423" = "C71620", # units
                          "C78426" = "C66726", # dose form
                          "C78425" = "C66729", # route
                          "C78745" = "C71113", # frequency
                          NA)
      
      response <- unique(cdash[cdash$Codelist.Code == term_code, "CDISC.Submission.Value"])
      
    } else {
      # Only other option is that it comes from a _PT2 tab
      response <- unique(pt2[, ref])
    } 
    
    tmp[tmp$Variable == var, "Response.Options"] <- paste(response, collapse = " | ")
    rm(response)
  }
  
  for (var in tmp$Variable[tmp$dataType == "Derived"]){
    response <- parse_response(tmp[tmp$Variable == var, "Response.Options"], field="derive")
    tmp[tmp$Variable == var, "Response.Options"] <- paste(response, collapse = " | ")
    rm(response)
  }
  
  return(tmp)
}

#-----------------------------------------------------
# Setup global arguments
#-----------------------------------------------------
argString <- commandArgs(trailingOnly = T)

usage <- paste("Usage: Rscript convert_dd_to_json.R
               -- Required Parameters --
               [-i | --infile]           <Path to the .xslx data dictionary> 
               [-d | --domain]           <Name of the sheet to convert> 
               -- Optional Parameters -- 
               [-o | --outfile]          <The filename for the output> (default = name of the -infile with a _<Sheet Name>.json suffix)
               [-p | --outpath]          <Path to the output directory> (default = the same directory as the --infile)
               [-s | --sdtm]             <Path to the .xlsx SDTM data dictionary export> (default = NULL)
               -- Optional Flags --   
               [-v | --verbose]          <Verbose output> (default = F)
               -- Help Flag --  
               [-h | --help]             <Displays this help message>
               Example:
               Rscript convert_dd_to_json.R -i connects_dd_v1.2.xlsx -s DM -o connects_dd_v1.2.json
               \n",sep="")

#0=no-arg, 1=required-arg, 2=optional-arg
spec <- matrix(c(	
  'domain',  'd', 1, "character",
  'infile',  'i', 1, "character",
  'outfile', 'o', 2, "character",
  'outpath', 'p', 2, "character",
  'sdtm',    's', 2, "character",
  'unittest','U', 0, "logical",
  'verbose', 'v', 0, "logical",
  'help',    'h', 0, "logical"
), byrow=TRUE, ncol=4);

if (length(argString) == 0){
  argString <- c( "-i", "C:/Users/jstratford/OneDrive - Research Triangle Institute/Documents/Projects/CONNECTS/20210901_convert_dd_to_json/CONNECTS_DD_V1.21_clean.xlsx", "-d", "VAC", "-v", "-s", "C:/Users/jstratford/OneDrive - Research Triangle Institute/Documents/Projects/CONNECTS/20210901_convert_dd_to_json/SDTM\ Terminology.xlsx" ) 
  #argString <- c( "-i", "C:/Users/jstratford/OneDrive - Research Triangle Institute/Documents/Projects/CONNECTS/20210901_convert_dd_to_json/UnitTests/MasterFile.xlsx", "-d", "Long", "-v", "-s", "C:/Users/jstratford/OneDrive - Research Triangle Institute/Documents/Projects/CONNECTS/20210901_convert_dd_to_json/SDTM\ Terminology.xlsx" )
  
  # AE CM COVID DM DS HO INT LB MH  ORG RSK SYM VS VAC 
}

args=getopt(spec, argString)

if ( !is.null(args$help) | is.null(args$infile) | is.null(args$domain) ) {
  add_to_log(lvl="error", func="getopt", message = "Either you asked for help or you are missing a required parameters: infile, domain, SDTM")
  add_to_log(lvl="error", func="getopt", message = usage, add = F)
  q(save="no",status=1,runLast=FALSE)
}

if(is.null(args$verbose)){args$verbose <- F}
if(is.null(args$sdtm)){args$sdtm <- NA}
if(is.null(args$outpath)){args$outpath <- dirname(args$infile)}
if(args$outpath == "."){args$outpath <- getwd()}
if(is.null(args$outfile)){args$outfile <- gsub(pattern = '.xlsx', paste0("_", args$domain,'.json'), basename(args$infile))}
if(is.null(args$unittest)){args$unittest <- F}
args$logfile <- gsub(pattern = '.xlsx', paste0("_", args$domain,'.log'), basename(args$infile))
args$datestamp <- format(Sys.Date(), format="%Y-%m-%d")
args$domain <- toupper(args$domain)

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
# UNIT TESTS
#-----------------------------------------------------
if (args$unittest){
  parse_response('[From CDASH controlled terminology "Units for Exposure" (C78423) to report the Protocol-Specific text response options. Examples: G, MG, ML, UG]', "from") #C78423
  parse_response('[DERIVE: "Y" FOR CMTRT ALL DRUGS WITH EXCEPTION OF "OTHER (SPECIFY)" AND PROTOCOL-SPECIFIC CMS OF INTEREST | "N" FOR "OTHER (SPECIFY)"]', "derive") # c("Y", "N")
  parse_response('[From CM_PT2 TAB: CATEGORY OF DRUG (CMCAT)]', "from") # CMCAT
  parse_response('[DATE: DD (2-DIGIT DAY OF THE MONTH)]', "date") # DD
  parse_response('[TIME: HH:MM (00:00-23:59)]', "time") # HH:MM
}

#-----------------------------------------------------
# Read in the SDTM Terminology
#-----------------------------------------------------

if (!is.na(args$sdtm)){
  sdtm <- read_SDTM(args$sdtm)  
}

#-----------------------------------------------------
# Read in the data dictionary
#-----------------------------------------------------
df <- read_dd(args$infile)

#-----------------------------------------------------
# Convert to json format and write to disk
#-----------------------------------------------------
keep_cols <- c("BDC.ID", "IP", "OP", "D", "N", "Element", "Variable", "Variable.Label", "Variable.Type", "Length", "Question","Response.Options", "Implementation.Notes")
out <- jsonlite::toJSON(df[, keep_cols], pretty = F, na = "null")

write_json(x=out, path=file.path(args$outpath, args$outfile))

if (args$domain == "VS"){
  # There is one field (VSORRES)in the VS domain whose
  #   responses vary depending on the value in
  #   the previous field (VSTEST)
  # So we will need a sub-dictionary and a
  #   special routine to process it
  pt2 <- read.xlsx(xlsxFile=args$infile, sheet=paste0(args$domain, "_pt2"), colNames=T, startRow = 3)
  out <- jsonlite::toJSON(pt2[, c("VSTEST", "VSORRES_format", "VSORRESU")], pretty = F)
  write_json(x=out, path=file.path(args$outpath, gsub("VS.json", "VS_pt2.json", args$outfile)))
}

if (args$domain == "LB"){
  # There is one field (VSORRES)in the VS domain whose
  #   responses vary depending on the value in
  #   the previous field (VSTEST)
  # So we will need a sub-dictionary and a
  #   special routine to process it
  pt2 <- read.xlsx(xlsxFile=args$infile, sheet=paste0(args$domain, "_pt2"), colNames=T, startRow = 3)
  out <- jsonlite::toJSON(pt2[, c("LBCAT", "LBSCAT", "LBTEST", "LBORRES_format", "LBORRESU", "LBORNRLO", "LBORNRHI")], pretty = F)
  write_json(x=out, path=file.path(args$outpath, gsub("LB.json", "LB_pt2.json", args$outfile)))
}


add_to_log(lvl = "info", func="main", message=paste0("Process began at ", init, " and finished at ", Sys.time()))
add_to_log(lvl = "info", func="main", message=paste0("Elapsed time: ", (proc.time() - timer)[['elapsed']]))
add_to_log(lvl = "info", func="main", message="Finished")
