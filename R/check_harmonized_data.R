rm(list=ls())
init <- Sys.time(); timer <- proc.time();
#-----------------------------------------------------
# This is a script perform a validation check
# to determine if uploaded data conforms with 
# the CONNECTS CDEs
# 
# Input: 
#   1. the csv formatted harmonized data
#   2. the json formatted data dictionary
#   3. What sheet do you want to check
#   4. What type of study this is
# 
# Output:
#   A tab delimited log file
#   An optional Excel Formatted workbook
#
# Author: Jeran Stratford
# Date: 02-SEP-2021
#-----------------------------------------------------

#-----------------------------------------------------
# Load required packages
#-----------------------------------------------------
if (!require(getopt)){install.packages('getopt', dependencies = T); library('getopt')}
if (!require(jsonlite)){install.packages('jsonlite', dependencies = T); library('jsonlite')}

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

check_domain <- function(domain = toupper(args$domain), dictionary){
  # Might want to change this in the future to pulling this from the json dictionary
  #acceptable_responses <- c("DM", "MH", "RSK", "ORG", "SYM", "AE", "CM", "VS", "LB", "COVID", "VAC", "DS", "HO")
  acceptable_responses <- toupper(dictionary)
  
  if (domain %!in% acceptable_responses){
    add_to_log(lvl="error", func="check_domain", message = paste0("Provided domain argument must be ", paste(toupper(acceptable_responses), collapse = ", ")), add = F)
    q(save="no",status=1,runLast=FALSE)
  } else {
    return(toupper(domain))
  }
}

check_study_type <- function(stype = toupper(args$study_type)){
  if (stype %!in% c("IP", "OP", "D", "N")){
    add_to_log(lvl="error", func="check_study_type", message = "Provided study type argument must be IP, OP, S, or N", add = F)
    q(save="no",status=1,runLast=FALSE)
  } else {
    return(toupper(stype))
  }
}

read_study_data <- function(fpath, filename, separator = ','){
  
  tmp <- tryCatch( 
    {
      read.table(file.path(fpath, filename), header = T, sep = separator, stringsAsFactors = F, quote = '"', colClasses = "character")
    },
    error = function(err) {
      message(paste("Unable to open file:", filename))
      message(err)
      return(NULL)
    }
  )
  return(tmp)
}

get_domain_dictionary <- function(filename = args$json_dictionary, domain = args$domain){
  tmp <- fromJSON(filename, flatten = T) 
  tmp <- fromJSON(tmp)
  check_domain(toupper(domain), dictionary = names(tmp))
  tmp <- fromJSON(tmp[[domain]])
  tmp <- as.data.frame(tmp)
  return(tmp)
}

update_passfail <-function(PF, newPF){
  
  # If the current status is "Warning"
  if (tolower(PF) == "warning"){
    if (tolower(newPF) == "warning"){out <- "Warning"} else if (newPF){out <- "Warning"} else if (!newPF){out <- F}
  } else if (PF){
    # If the current state is Pass
    if (tolower(newPF) == "warning"){out <- "Warning"} else if (newPF){out <- T} else if (!newPF){out <- F}
  } else {
    # If the current state is False
    out = F
  }
  return(out)
}

use_your_words <- function(x){
  if (is.na(x)){
    out <- "Absent"
  } else if (tolower(x) == "warning") {
    out <- "Warning"
  } else if (x){
    out <- "PASS"
  } else if ( isFALSE(F) ){
    out <- "FAIL"
  }
  return(out)
}

is_na_string <- function(x){
  # There are multiple ways of listing an NA
  if (!is.na(x) & toupper(x) %in% c("N/A", "[N/A]", "NA", "", ".", "NAN", "<NA>")){x <- NA}
  return(x)
}

getType <- function(vartype, response){
  if (!is.na(response) && substr(x = tolower(response), 1, 12) == "[conditional"){
    out <- "Conditional"
  } else if (tolower(vartype) == "num"){
    out <- "Numeric"
  } else if (substr(x = tolower(response), 1, 6) == "[date:"){
    out <- "Date"
  } else if (substr(x = tolower(response), 1, 6) == "[time:"){
    out <- "Time"
  } else {
    out <- "Character"
  }
  return(out)
}

isCaseSensitive <- function(x){
  x <- is_na_string(x)
  grepl("This field is case sensitive.", x, ignore.case = T)
}

isFlexible <- function(x){
  x <- is_na_string(x)
  grepl("one order of magnitude", x, ignore.case = T)
}

#----------------------------------
# Numeric fields
#----------------------------------
parse_numeric_format <- function(x){
  # What is the format is left blank
  if (is.na(x) | x == "(any)"){
    # Not going to put any restriction on the number of digits
    out <- c(Inf,Inf)
  } else {
    x <- strsplit(as.character(x), "_")[[1]]
    if(length(x) == 1){
      # This is just an integer length format
      out <- c(x, 0)
    } else {
      # This would be for traditional SAS outputs (##_##)
      out <- x
    }
  }
  
  return( out ) 
}

decimal_places <- function(x) {
  # What do we want to do about trailing 0's???
  # sub('0+$', '', as.character(x))
  
  x <- as.character(x)
  
  if ( gsub("[0-9]", "", x) == "" ) {
    # If could be an integer response 
    out <- c(nchar(x), 0)
  } else if ( gsub("[0-9]", "", x) == "."){
    # it could be a float 
    out <- nchar( strsplit(x, ".", fixed=TRUE)[[1]] )
  } else {
    # There could be something besides numeric values
    # or perhaps it has extra characters like commas
    out <- c(NA, NA)
  }
  
  return( out )
}

check_numeric <- function(fieldname, data, criteria, verbose=args$verbose, suppress=F){
  # Remove missing values from the field
  # data <- c(seq(0,1,0.1), 5, 10, "1,000", "1.0.1")
  data <- unlist(lapply(data, function(x) is_na_string(x)))
  data <- data[!is.na(data)]
  
  # Are there any values remaining to check???
  if (length(data) == 0){
    # There is no data, but there are no failures either
    passfail <- TRUE
  } else {
    #-------------------------------------
    # Are there any non-numeric values
    #-------------------------------------
    field_as_numeric <- suppressWarnings(as.numeric(data))
    
    if ( any(is.na(field_as_numeric)) ){
      
      message_text <- paste("Field:", fieldname, "contains non-numeric values")
      if (verbose){
        message_text <- paste0(message_text, ":", paste(unique(data[is.na(field_as_numeric)]), collapse = ", ") )  
      }
      
      if (!suppress){
        add_to_log(lvl = "error", func="Numeric Variables", message=message_text)   
      }
      
      passfail <- FALSE
    } else {
      passfail <- TRUE
    }
    
    #-------------------------------------
    # Check the format for all numeric 
    #   variables
    #-------------------------------------
    numformat <- sapply(data[!is.na(field_as_numeric)], function(x) decimal_places(x) )
    
    if (any(is.na(numformat))){
      if (!suppress){
        add_to_log(lvl = "error", 
                   func="Numeric variables", 
                   message=paste("Field:", fieldname, "contains more than numeric characters or a single decimal point") 
        )
      }
      passfail <- update_passfail(passfail, FALSE)
    }
    
    #-------------------------------------
    # Check the decimal places
    #-------------------------------------
    left_of_decimal = as.numeric(criteria[1]) >= numformat[1,!is.na(numformat[1,])]
    right_of_decimal = as.numeric(criteria[2]) >= numformat[2,!is.na(numformat[2,])]
    
    passfail <- update_passfail(passfail, all(c(left_of_decimal, right_of_decimal)))
    
    if (!all(left_of_decimal) & !suppress){
      add_to_log(lvl = "error", 
                 func="Numeric variables", 
                 message=paste("Field:", fieldname, "has too many digits left of the decimal.") 
      )  
    } 
    if (!all(right_of_decimal)  & !suppress) {
      add_to_log(lvl = "error", 
                 func="Numeric variables", 
                 message=paste("Field:", fieldname, "has too many digits right of the decimal.") 
      )
    }
  }
  return(passfail)
}

#----------------------------------
# Date fields
#----------------------------------
IsDate <- function(x, delim, date.format, responses){
  
  if (delim == ""){
    # We're only expecting a single date component
    x <- tolower(x)
  } else {
    # We have a delimited string with multiple date components 
    x <- tolower(strsplit(x, split = delim, perl = T)[[1]])
  }
  
  if (length(x) > nrow(date.format)){
    # You have more delimited fields than expected
    passfail <- F
  } else if (length(x) == nrow(date.format)){
    # Check each field
    passfail <- T
    for (i in seq_along(date.format$position)){
      passfail <- update_passfail(passfail, x[i] %in% unlist(responses[date.format[i, "dformat"]]) )
    }
  } else if (length(x) < nrow(date.format)){
    # Try any of the truncated versions of the expected format
    passfail <- T
    
    # Drop out the day component
    # This may already be absent, would just be a repeat of previous test
    tmp <- date.format[date.format$component %!in% c("day"),]
    for (i in seq_along(tmp$position)){
      passfail <- update_passfail(passfail, x[i] %in% unlist(responses[tmp[i, "dformat"]]) )
    }
    
    if (!passfail){
      # Drop out the day and month component
      tmp <- date.format[date.format$component %!in% c("day", "month"),]
      for (i in seq_along(tmp$position)){
        passfail <- x[i] %in% unlist(responses[tmp[i, "dformat"]]) & length(x) == nrow(tmp)
      } 
    }
  }
  
  return(passfail)
}

find_date_format <- function(x){
  # Valid R Date formats
  #Symbol	Meaning	                Example
  #%d	    day as a number (0-31)	01-31
  #%a     abbreviated weekday 	  Mon
  #%A	    unabbreviated weekday   Monday
  #%m	    month (00-12)	          00-12
  #%b     abbreviated month       Jan
  #%B	    unabbreviated month     January
  #%y     2-digit year            07
  #%Y	    4-digit year            2007
  
  # What are where are the delimiters
  delim = strsplit(gsub("[ [:alnum:] ]", "", x),"")[[1]]
  
  if (length(delim) > 0){
    delim_str <- tolower(strsplit(x, delim)[[1]])
  } else {
    # This is a single field and has no delimiter
    delim_str <- tolower(x)
  }
  
  out <- setNames(data.frame(matrix(ncol = 4, nrow = length(delim_str)), stringsAsFactors = F), c("position", "component", "dformat", "lbl"))
  
  for (i in seq_along(delim_str)){
    
    out[i, "position"] = i
    out[i, "lbl"] = delim_str[i]
    
    if (substr(delim_str[i], 1, 1) == 'd'){
      out[i, "component"] = 'day'
      out[i, "dformat"] = "%d"
    } else if ( substr(delim_str[i], 1, 1) == 'm' ) {
      out[i, "component"] = 'month'
      if (nchar(delim_str[i]) < 3){
        out[i, "dformat"] = "%m"
      } else if (nchar(delim_str[i]) == 3){
        out[i, "dformat"] = "%b"
      } else if (nchar(delim_str[i]) > 3){
        out[i, "dformat"] = "%B"
      }
    } else if ( substr(delim_str[i], 1, 1) == 'y' ){
      out[i, "component"] = 'year'
      if (nchar(delim_str[i]) == 2){
        out[i, "dformat"] = "%y"
      } else if (nchar(delim_str[i]) == 4){
        out[i, "dformat"] = "%Y"
      }
    }
  }
  
  return( out )
}

check_date <- function(fieldname, data, criteria = "dd-mon-yyyy"){
  # Parse out the date format
  criteria <- gsub(pattern = "^\\[DATE:", "", criteria, perl = T)
  criteria <- gsub(pattern = "\\]$", "", criteria, perl = T)
  criteria <- strsplit(x = criteria, split = "\\(", perl = T)[[1]][1]
  criteria <- trimws(criteria)
  
  # Remove missing values from the field
  # Some studies code missing with NA, other just list it as the empty string
  data <- unlist(lapply(data, function(x) is_na_string(x)))
  data <- data[!is.na(data)]
  
  # Are there any values remaining to check???
  if (length(data) == 0){
    # There is no data, but there are no failures either
    passfail <- TRUE
  } else {
    
    # Since we control the data dictionary we're asserting that we won't mix delimiters
    delim = unique(strsplit(gsub("[ [:alnum:] ]", "", criteria),"")[[1]])
    if (length(delim) < 1){
      # For when the date field is just a component (like dd)
      delim <- ""
    } else if (length(delim) > 1){
      message("Parsing date format failed")
    }
    
    
    dFormat <- find_date_format(criteria)
    
    # I'm assuming that there will be no dates prior to 1900
    now <- format(Sys.Date(), "%Y")
    responses <- list("%d" = format(ISOdate(2021,1,1:31),"%d"),
                      "%m" = format(ISOdate(2021,1:12,1),"%m"),
                      "%b" = tolower(format(ISOdate(2021,1:12,1),"%b")),
                      "%B" = tolower(format(ISOdate(2021,1:12,1),"%B")),
                      "%y" = unique(format(ISOdate(1900:now,1,1),"%y")),
                      "%Y" = format(ISOdate(1900:now,1,1),"%Y"))
    
    charformat <- sapply(data, function(x) IsDate(x, delim, dFormat, responses))
    
    if ( all( charformat ) ){
      passfail <- TRUE
    } else {  
      add_to_log(lvl = "error", 
                 func="Date Variables", 
                 message=paste("Field:", fieldname, "has entries that do not have the following acceptable date formats:", paste(dFormat$lbl, collapse = delim), "or its derivatives") 
      ) 
      passfail <- FALSE
    }
  }
  
  return(passfail)
}

#----------------------------------
# Time fields
#----------------------------------
IsTime <- function(myTime, time_format){
  
  # Time formats will always be colon separated components
  # with a dash to indicate ranges in parenthesis
  # (HH:MM-HH:MM) or (HH:MM:SS-HH:MM:SS)
  fmt <- trimws( substr(time_format, 1, gregexpr(pattern = "(", text = time_format, fixed = T)[[1]][1] - 1) )
  bounds <- trimws( substr(time_format, gregexpr(pattern = "(", text = time_format, fixed = T)[[1]][1] + 1, nchar(time_format) -1) )
  
  format_length <- nchar(fmt)
  field_count <- length(strsplit(x=fmt, split = ":")[[1]])
  
  bounds <- lapply(strsplit(bounds, split = "-", fixed = T), function(x) strsplit(x, ":", fixed = T))[[1]]
  names(bounds) <- c("start", "end")
  
  if (nchar(myTime) == format_length){
    myTime <- strsplit(myTime, ":")[[1]]
    
    if (length(myTime) == field_count){
      
      out <- T
      for (i in seq_len(field_count)){
        out <- tryCatch(update_passfail(out, as.numeric(myTime[i]) %in% seq(bounds$start[i],bounds$end[i],1)) , 
                        error = function(err) {update_passfail(out, FALSE)}
        )  
      }
      
    } else {
      out <- FALSE
    }
  } else {
    out <- FALSE
  }
  return(out)
}

check_time <- function(fieldname, data, criteria = "hh:mm (00:00-23:59)"){
  # Parse out the time format
  criteria <- gsub(pattern = "^\\[TIME:", "", criteria, perl = T)
  criteria <- gsub(pattern = "\\]$", "", criteria, perl = T)
  criteria <- trimws(criteria)
  
  # Some studies code missing with NA, other just list it as the empty string
  data <- unlist(lapply(data, function(x) is_na_string(x)))
  data <- data[!is.na(data)]
  
  ### Is this an appropriate place to update record counts on field_status?
  
  if (length(data) == 0){
    passfail <- TRUE
  } else {
    if ( all( sapply(data, function(x) IsTime(x, criteria)) ) ){
      passfail <- TRUE
    } else {  
      add_to_log(lvl = "error", 
                 func="Time Variables", 
                 message=paste("Field:", fieldname, "does not have the format", criteria)) 
      passfail <- FALSE
    }
  }
  
  return(passfail)
}

#----------------------------------
# Character fields
#----------------------------------
IsRegulatedChar <- function(x, criteria, case_sensitive = F){
  #----------------------------------
  # Determine field criteria 
  #----------------------------------
  if (length(criteria) == 1 && tolower(substr(criteria, 1, 1)) == '['){
    # Free text field
    # Just set the criteria to the value
    criteria <- x
  } 
  
  others <- grepl(pattern = "^OTHER, \\[SPECIFY\\]$", x=toupper(criteria), perl = T)
  if (any(others)){
    #criteria <- gsub(pattern = "^OTHER, \\[SPECIFY\\]$", replacement = "OTHER,", x = criteria, perl = T)
    criteria[others] <- substr(criteria[others], 1, 6)
    
    if (tolower(substr(x, 1, 6)) == "other,"){x <-  substr(x, 1, 6)}
  }
  
  if (!case_sensitive) { 
    criteria <- tolower(criteria)
    x <- tolower(x)
  }
  
  return (x %in% criteria)
}

check_char <- function(fieldname, data, criteria, field_len, check_case = F, verbose = args$verbose, suppress=F){
  
  #--------------------------------------------
  # Process the criteria 
  #--------------------------------------------
  if (is.na(criteria)){
    # There are some fields without any responses
    criteria <- '[EMPTY]'
    message(paste(fieldname, "has no reported response options, treating as freetext"))
  } else {
    criteria <- trimws(strsplit(criteria, split = "|", fixed=TRUE)[[1]])
    criteria <- trimws(iconv(criteria, from = 'UTF-8', to = 'ASCII//TRANSLIT')) # The character with ASCII code 160 is called a "non-breaking space."
  }
  
  #--------------------------------------------
  # Process the data
  #--------------------------------------------
  data <- unlist(lapply(data, function(x) is_na_string(x))) # Remove missing values from the field
  data <- data[!is.na(data)]
  data <- trimws(data) # Cleanup white space
  
  if (length(data) == 0){ # Are there any values remaining to check???
    passfail <- TRUE
  } else {
    
    #------------------------------------------
    # Check the length of the field
    #------------------------------------------
    len_check <- sapply(data, function(x) as.numeric(field_len) >= nchar(x))
    if ( all(len_check) ){
      passfail <- TRUE
    } else {
      passfail <- FALSE
      if (!suppress){
        add_to_log(lvl = "error", 
                   func="Character Variables", 
                   message=paste("Free text character field:", fieldname, "exceeds specified length", paste0("(",field_len,")"))) 
      }
    }
    
    #------------------------------------------
    # Check the response options
    #------------------------------------------
    response_check <- sapply(data, function(x) IsRegulatedChar(x = x, criteria = criteria, case_sensitive = check_case))
    if ( all(response_check) ){
      passfail <- update_passfail(passfail, TRUE)
    } else {
      passfail <- update_passfail(passfail, FALSE)
      
      message_text <- paste("Field:", fieldname, "contains unacceptable responses")
      if (verbose){
        message_text <- paste0(message_text, ":", paste(unique(data[!response_check]), collapse = ", ") )  
      }
      
      if (!suppress){
        add_to_log(lvl = "error", func="Character Variables", message=message_text)  
      }
    }
    
    #------------------------------------------
    # Report what "Other" fields were supplied 
    #------------------------------------------
    if (any(grepl(pattern = "^OTHER, \\[SPECIFY\\]$", x=toupper(criteria), perl = T))){
      msg_vals <- unique(trimws(sapply(data[grepl(pattern = "^OTHER,", x = toupper(data), ignore.case = T, perl = T)], function(x) substr(x, 7, nchar(x)))))
      if (length(msg_vals) > 0 ){
        message_text <- paste("Field:", fieldname, "reports 'Other' responses:", paste(msg_vals, collapse = ", "))
        add_to_log(lvl = "info", func="Character Variables", message=message_text)   
      }
    }
  }
  return(passfail)
}

check_conditional <- function(tmp, cdict, field_type, field_format, flexible = F, check_case = F, verbose = args$verbose){
  ###############################################################
  # tmp is the 2 column data frame of study data
  #   1. the value to reference upon
  #   2. the conditional response
  # For a given 1, there are only finite values that 2 can take
  ###############################################################
  # cdict is a 2 column conditional data dictionary
  #   1. the referent column
  #   2. the available responses
  ###############################################################
  
  # Remove rows with missing data
  tmp[,2] <- unlist(sapply(tmp[,2], function(x) is_na_string(x)))
  
  if ( sum(is.na(tmp[,2])) > 0 ){
    add_to_log(lvl = "warning", 
               func="Conditional Char", 
               message=paste("Field:", colnames(tmp)[2], "is reporting", sum(is.na(tmp[,2])), "NULL values across referent(s):", paste(unique(tmp[which(is.na(tmp[,2])),1]), collapse = ", "))) 
  }
  
  tmp <- tmp[!is.na(tmp[,2]),]
  
  # Check the referent field
  ref_in_dict <- tolower(tmp[,1]) %in% tolower(cdict[,1])
  if (sum(!ref_in_dict) > 0){
    
    add_to_log(lvl = "error", 
               func="Conditional Char", 
               message=paste("Referrent Field", paste0("(", colnames(tmp)[1], ")"), "response(s) is/are not in the conditional dictionary and validation of conditional field", paste0("(", colnames(tmp)[2],")"), "will not be evaluated for rows reporting:", paste(unique(tmp[!ref_in_dict, 1]), collapse = ", "))) 
    
    tmp <- tmp[ref_in_dict,]
  }
  
  # Find the largest and then use it to reuse check_char function
  derived_len <- max(unlist(lapply(lapply(sapply(cdict[,2], function(x) strsplit(x, "|", fixed=T)), function(x) nchar(x)), function(x) max(x))), na.rm = T)
  
  # You can't have a conditional response (column 2) 
  # without a value to reference (column 1)
  if (any(is.na(tmp[ sapply(tmp[,2], function(x) !is.na(x)), 1]))){
    add_to_log(lvl = "error", 
               func="Conditional Char", 
               message=paste("Field:", colnames(tmp)[2], "is reporting a conditional response without a reference in", colnames(tmp)[1])) 
    passfail <- FALSE
  } else {
    passfail <- TRUE
  }
  
  tmp$valid <- NA
  for (i in seq_len(nrow(tmp))){
    
    criteria <- cdict[ toupper(cdict[,1]) == toupper(tmp[i,1]), 2 ]
    criteria <- is_na_string(criteria)
    
    if (!is.na(criteria) & grepl(pattern = "^[0-9]+_[0-9]+$", x = criteria, perl = T)) {
      
      # Sometimes the field is numeric but we are looking for a specific value (Lab Results Hi/Lo range)
      # So we don't want to check those as numeric but as regulated character
      # If the user has converted the ranges based on non-preferred units then we'd want to run them as numeric
      # But I haven't implemented that as we are trying to steer people towards preferred units
      # There is a parameter for passing field length so that those numeric formats on the main DD sheet can be examined
      # Sometimes the field is listed character (VSORRES) but is actually reporting numeric for some tests (Height, Weight, BMI)
      
      #-----------------------------------------------
      # Certain fields (lab results) we want to allow 
      # for a 10-fold increase/decrease in reported 
      # values without throwing an error
      # Beyond that we want to throw a warning flag
      #-----------------------------------------------
      criteria = parse_numeric_format(criteria)
      if (flexible){
        # Allow one order of magnitude in from of the decimal place
        criteria[1] <- as.numeric(criteria[1]) + 1
        
        # We want to allow the studies to report any amount of decimal places after the decimal place 
        criteria[2] <- Inf
      }
      
      # Then its a numeric field  
      tmp[i,"valid"] <- check_numeric(fieldname = paste(colnames(tmp)[2], tmp[i,1], sep="_"), #, paste0("Row",i), 
                                      data = tmp[i,2], 
                                      criteria = criteria, 
                                      verbose = verbose, 
                                      suppress = !verbose)
    } else {
      
      if (is.na(criteria)){
        # Expecting a null response
        tmp[i,"valid"] <- is.na(is_na_string(tmp[i,2]))
      } else {
        # Then is a regulated response character field
        tmp[i,"valid"] <- check_char(fieldname = paste(colnames(tmp)[2], tmp[i,1], sep="_"),
                                     data = tmp[i,2], 
                                     criteria = criteria, 
                                     field_len = derived_len, 
                                     check_case = check_case,
                                     verbose = verbose, 
                                     suppress = !verbose )  
      }
    }
  }
  
  if (all(tmp$valid)){
    passfail <- update_passfail(passfail, TRUE)
  } else {
    message_text <- paste("Field:", colnames(tmp)[2], "has unacceptable responses for variables", paste(unique(tmp[!tmp$valid, 1]), collapse = ", ") )
    if (verbose){
      message_text <- paste(message_text, "on rows", paste(which(!tmp$valid), collapse = ","))  
    }
    
    add_to_log(lvl = "error", 
               func="Conditional Char", 
               message=message_text) 
    
    if (flexible) {
      passfail <- update_passfail(passfail, "Warning")
    } else {
      passfail <- update_passfail(passfail, FALSE)  
    }
    
  }
  
  return(passfail)
} 

#----------------------------------
# Missing Data
#----------------------------------
count_missing <- function(x){
  # Missing can be reported in a few different ways
  # I will convert them all to NA 
  # Then count only fields that are not NA
  
  # Sometimes studies report missing as the empty string
  # Sometimes studies report missing as "."
  x <- unlist(lapply(x, function(x) is_na_string(x)))
  
  return( sum(is.na(x), na.rm = T) )
}

#----------------------------------
# Output Formatting
#----------------------------------
make_pretty_txt_table <- function(x, len){
  ###
  # This function just pads columns so that they align when viewing
  # the tab delimited file in a text editor without parsing
  # This is helpful since we doubt that excel is available in the BDC staging area
  ###
  
  if (len < 8){
    # If the longest str in the column is less than 8 characters then do nothing
    out <- x
  } else {
    seps = ceiling(len / 8) - floor(nchar(x) / 8) - 1
    if (len %% 8 == 0){
      seps = seps + 1
    }
    
    for (i in seq_along(seps)){ seps[i] <- paste( rep("\t", seps[i]), collapse="") }
    
    out <- paste0(x, seps)
  }
  return(out)
}

#-----------------------------------------------------
# Setup global arguments
#-----------------------------------------------------
argString <- commandArgs(trailingOnly = T)

usage <- paste("Usage: Rscript convert_dd_to_json.R
               -- Required Parameters --
               [-i | --infile]           <Path to the .csv formatted harmonized data> 
               [-j | --json_dictionary]  <Path to the .json formatted data dictionary> 
               [-d | --domain]           <The ID of the domain that is to be checked>
               [-s | --study_type]       <What type of study is this?> (Options: IP, OP, D, N)
               -- Optional Parameters -- 
               [-o | --outfile]          <The filename for the output log file> (default = <YYYY-MM-DD>_CDE_harmonization_validator_<Domain>.log)
               [-p | --outpath]          <Path to the output directory> (default = the same directory as the --infile)
               [-E | --excel]            <Create an excel formatted output log) (default = F)
               [-c | --conditional_dd]   <Name of the conditional data dictionary> (default = '')
               -- Optional Flags --   
               [-V | --verbose]          <Verbose output> (default = F)
               -- Help Flag --  
               [-h | --help]             <Displays this help message>
               Example:
               Rscript check_harmonized_data.R -i DM.csv -d DM -j connects_dd_v1.2_DM.json -s IP
               \n",sep="")

#0=no-arg, 1=required-arg, 2=optional-arg
spec <- matrix(c(	
  'conditional_dd',  'c', 2, "character",
  'excel',           'E', 0, "logical",
  'infile',          'i', 1, "character",
  'json_dictionary', 'j', 1, "character",
  'domain',          'd', 1, "character",
  'outfile',         'o', 2, "character",
  'outpath',         'p', 2, "character",
  'study_type',      's', 1, "character",
  'unittest',        'U', 0, "logical",
  'verbose',         'V', 0, "logical",
  'help',            'h', 0, "logical"
), byrow=TRUE, ncol=4);
                     
args=getopt(spec, argString)

if ( !is.null(args$help) | is.null(args$infile) | is.null(args$json_dictionary) | is.null(args$domain) | is.null(args$study_type)) {
  add_to_log(lvl="error", func="getopt", message = "Either you asked for help or you are missing a required parameters: infile, json_dictionary, domain, study_type", add = F)
  add_to_log(lvl="error", func="getopt", message = usage, add = F)
  q(save="no",status=1,runLast=FALSE)
}

args$datestamp <- format(Sys.Date(), format="%Y-%m-%d")
if(is.null(args$outpath)){
  if (dirname(args$infile) == "."){
    args$outpath <- getwd()
  } else {
    args$outpath <- dirname(args$infile)
  }
}
if(is.null(args$outfile)){args$outfile <- paste0(args$datestamp, "_CDE_harmonization_validator_", args$domain, ".log")}
if(is.null(args$conditional_dd)){args$conditional_dd <- ""}
if(is.null(args$excel)){args$excel <- F}
if(is.null(args$unittest)){args$unittest <- F}
if(is.null(args$verbose)){args$verbose <- F}

#-----------------------------------------------------
# Check arguments for acceptable responses
#-----------------------------------------------------
args$study_type <- check_study_type()

#----------------------------------------------------#
#----------------------------------------------------#
#                 Method Execution                   #
#----------------------------------------------------#
#----------------------------------------------------#

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
# Read in the .json formatted data dictionar(y/ies)
#-----------------------------------------------------
dd <- get_domain_dictionary(filename = args$json_dictionary, domain = args$domain)

if (args$conditional_dd != ""){
  conditional_dd <- get_domain_dictionary(filename = args$json_dictionary, domain = args$conditional_dd)  
}

#-----------------------------------------------------
# Read in the .csv formatted harmonized data
#-----------------------------------------------------
df <- read_study_data(fpath = dirname(args$infile), filename = basename(args$infile), separator = ',')

if (is.null(df)){
  add_to_log(lvl = "error", func="main", message=paste("Unable to open", args$infile)) 
} else {
  add_to_log(lvl = "info", func="main", message=paste(nrow(df), "observations reported for", length(unique(df$SUBJID)), "subject(s) at", length(unique(df$SITEID)), "site(s) across", length(unique(df$STUDYID)), "study(ies):", paste(unique(df$STUDYID), collapse = ", "))) 
}

#-----------------------------------------------------
# Setup the output pass/fail table
#-----------------------------------------------------
available_columns = dd$Variable[!is.na(dd$BDC.ID)]
expected_columns = dd$Variable[!is.na(dd$BDC.ID) & dd[,args$study_type] == "C"]

field_status <- data.frame("Domain" = args$domain,
                           "Field" = available_columns,
                           "Requirement" = dd[dd$Variable %in% available_columns,args$study_type],
                           "Type" = sapply(available_columns, function(x) getType(dd$Variable.Type[dd$Variable == x], dd$Response.Options[dd$Variable == x])),
                           "Validation_Status" = NA, 
                           "Total_Records" = NA,
                           "Percent_Records_Missing" = NA,
                           stringsAsFactors = F)

#-----------------------------------------------------
# Expected fields
#-----------------------------------------------------
if ( sum(expected_columns %!in% colnames(df)) > 0 ){
  tmp <- expected_columns[expected_columns %!in% colnames(df)]
  
  add_to_log(lvl = "error", 
             func="Core variables", 
             message=paste("Expected core fields are missing:", paste(tmp, collapse=", ")) 
  )
  
  for (field in tmp){
    field_status[field_status$Field == field, "Validation_Status"] = "Warning"  
    field_status[field_status$Field == field, "Total_Records"] = 0  
    field_status[field_status$Field == field, "Percent_Records_Missing"] = 0  
  }
  
  # This will allow us to skip these variables later on
  expected_columns <- expected_columns[expected_columns %in% colnames(df)]
}

#-----------------------------------------------------
# Extra unexpected fields
#-----------------------------------------------------
if (sum(colnames(df) %!in% available_columns) > 0 ){
  tmp <- unique(colnames(df)[colnames(df) %!in% available_columns])
  
  add_to_log(lvl = "warning", 
             func="Unexpected variables", 
             message=paste("Unexpected fields:", paste(tmp, collapse=", ")) 
  )
}

#-----------------------------------------------------
# Check numeric fields
#-----------------------------------------------------
num_fields <- colnames(df)[colnames(df) %in% field_status[field_status$Type == "Numeric", "Field"]]
for (field in num_fields){
  field_status[field_status$Field == field, "Validation_Status"] = 
    check_numeric(fieldname = field, 
                  data = df[,field], 
                  criteria = parse_numeric_format(dd[dd$Variable == field, "Length"]))
}

#-----------------------------------------------------
# Check date fields
#-----------------------------------------------------
date_fields <- colnames(df)[colnames(df) %in% field_status[field_status$Type == "Date", "Field"]]
for (field in date_fields){
  field_status[field_status$Field == field, "Validation_Status"] = 
    check_date(fieldname = field, 
               data = df[,field], 
               criteria = dd[dd$Variable == field, "Response.Options"])
}

#-----------------------------------------------------
# Check time fields
#-----------------------------------------------------
time_fields <- colnames(df)[colnames(df) %in% field_status[field_status$Type == "Time", "Field"]]
for (field in time_fields){
  field_status[field_status$Field == field, "Validation_Status"] = 
    check_time(fieldname = field, 
               data = df[,field], 
               criteria = dd[dd$Variable == field, "Response.Options"])
}

#-----------------------------------------------------
# Check character fields
#-----------------------------------------------------
char_fields <- colnames(df)[colnames(df) %in% field_status[field_status$Type == "Character", "Field"]]
for (field in char_fields){
  field_status[field_status$Field == field, "Validation_Status"] = 
    check_char(fieldname = field, 
               data = df[,field], 
               criteria = dd[dd$Variable == field, "Response.Options"],
               field_len = dd[dd$Variable == field, "Length"], 
               check_case = isCaseSensitive(dd[dd$Variable == field, "Implementation.Notes"]))
}

#-----------------------------------------------------
# Check Conditional Character fields
#-----------------------------------------------------
cond_fields <- colnames(df)[colnames(df) %in% field_status[field_status$Type == "Conditional", "Field"]]
for (field in cond_fields){
  # What is the referent field
  referent <- tail(strsplit(gsub("[\\[\\]]", "", dd[dd$Variable == field, "Response.Options"], perl = T), " ")[[1]], 1)
  
  # Identify the field in the conditional dd to use
  a <- grep(pattern = paste0("^", field, "$"), names(conditional_dd), perl = T)
  b <- grep(pattern = paste0("^", field, "_"), names(conditional_dd), perl = T)
  if (length(a) == 1){
    condition <- names(conditional_dd)[a]
  } else if (length(b) == 1){
    condition <- names(conditional_dd)[b]
  } else {
    add_to_log(lvl = "error", 
               func="Conditional Variables", 
               message=paste("Field:", field, "doesn't have a singular corresponding column in the conditional dictionary"))
    field_status[field_status$Field == field, "Validation_Status"] <- F
  }
  
  if (exists("condition")){
    field_status[field_status$Field == field, "Validation_Status"] = 
      check_conditional(tmp = df[,c(referent, field)],
                        cdict = conditional_dd[c(referent, condition)],
                        field_type = dd[dd$Variable == field, "Variable.Type"],
                        field_format = dd[dd$Variable == field, "Length"],
                        flexible = isFlexible(dd[dd$Variable == field, "Implementation.Notes"]),
                        check_case = isCaseSensitive(dd[dd$Variable == field, "Implementation.Notes"]))
  }
  
  rm(referent, condition, a, b)
}

#-----------------------------------------------------
# Calculate missing values counts
#-----------------------------------------------------
for (field in available_columns){
  if (field %in% colnames(df)){
    field_status[field_status$Field == field, "Total_Records"] = nrow(df)
    field_status[field_status$Field == field, "Percent_Records_Missing"] = round( count_missing(x = df[,field]) / nrow(df), 3) * 100
  } else {
    field_status[field_status$Field == field, "Total_Records"] = 0
    field_status[field_status$Field == field, "Percent_Records_Missing"] = 0
  }
}

#-----------------------------------------------------
# Check for missing values in core fields
#-----------------------------------------------------
for (field in field_status$Field[field_status$Requirement == "C"]){
  if (field_status[field_status$Field == field,"Percent_Records_Missing"] > 5){
    add_to_log(lvl = "warning", 
               func="Missing Values in Core field", 
               message=paste(field, "is a core field but has more than 5% of the entries missing"))
    
    field_status[field_status$Field == field,"Validation_Status"] <- 
      update_passfail(field_status[field_status$Field == field,"Validation_Status"], "Warning")
  }
}

#-----------------------------------------------------
# Format the output log file
#-----------------------------------------------------
# Thus far the log has been updated in real time
# this is in case there is a failure along the way
# now lets reformat the log and overwrite the file
# that has been written to disk
log_header <- paste("CDE Harmonization Validator", "|",
                    "Domain:", args$domain, "|",
                    "Run by:", Sys.info()[['effective_user']], "|",
                    "On:", args$datestamp, "|", 
                    "Environment:", Sys.info()[['nodename']], "|",
                    sessionInfo()$R.version$version.string
)

colnames(field_status) <- gsub("Validation_Status", "Status", colnames(field_status))
field_status$Status <- sapply(field_status$Status, function(x) use_your_words(x))

# Get the maximum number of characters in the fields or column name
column_lengths <- lapply(colnames(field_status), function(x) max(nchar((c(x, field_status[,x])))) )
names(column_lengths) <- colnames(field_status)

for (field in colnames(field_status)){
  field_status[,field] <- sapply(field_status[,field], function(x) make_pretty_txt_table(x, unlist(column_lengths[field])))
}

#-----------------------------------------------------
# Write the harmonization summary to disk
#-----------------------------------------------------
# Singular Header Row
fileConn<-file(file.path(args$outpath, args$outfile) )
writeLines(log_header, fileConn)
close(fileConn)

# Header of 
table_header <- sapply(colnames(field_status), function(x) make_pretty_txt_table(x, len = unlist(column_lengths[x])))
write(paste(gsub("_", " ", table_header), collapse = "\t"), file.path(args$outpath, args$outfile), append=TRUE)

# Pass/Fail Summary table
write.table(x = field_status, file = file.path(args$outpath, args$outfile), col.names = F, row.names = F, quote = F, append=TRUE, sep ='\t')

# log statements for fields that are not valid
#if (args$verbose){
log_out = logbook[, "message"]
#} else {
#  log_out = logbook[logbook$lvl %in% c("WARNING", "ERROR"), "message"]
#}
write.table(x = log_out, 
            file = file.path(args$outpath, args$outfile), 
            col.names = F, 
            row.names = F, 
            quote = F, 
            append=TRUE, 
            sep ='\t')


if (args$excel){
  if (!require(openxlsx)){install.packages('openxlsx', dependencies = T); library('openxlsx')}
  
  # Define format styles
  centered_style <- createStyle(halign = "center")
  table_header <- createStyle(halign = "center", fgFill = "#DDDDDD", textDecoration = c("BOLD"))
  posStyle <- createStyle(fontColour = "#000000", bgFill = "#48f048")
  warnStyle <- createStyle(fontColour = "#000000", bgFill = "#FFC000")
  negStyle <- createStyle(fontColour = "#000000", bgFill = "#f72f2f")
  
  # Does the summary file already exist?
  excel_filename <- gsub(pattern = paste0(args$domain, ".log", "$"), replacement = "summary.xlsx", args$outfile, perl = T)
  # Going to need something like this if the user specifies an output filename
  #gsub(paste0(".", tail(strsplit(x = args$outfile, split = ".", fixed = T)[[1]],1), "$"), "_summary.xlsx", args$outfile, perl = T)
  
  if (!file.exists(file.path(args$outpath, excel_filename))){
    wb <- createWorkbook("results")  
  } else {
    wb = loadWorkbook(file.path(args$outpath, excel_filename))
  }
  
  # If the domain already exist, delete it 
  if ( args$domain %in% sheets(wb) ){
    removeWorksheet(wb, args$domain)
  }
  
  addWorksheet(wb, args$domain)
  writeData(wb = wb, sheet = args$domain, startCol = 1, startRow = 1, x = log_header)
  writeData(wb = wb, sheet = args$domain, startCol = 1, startRow = 2, x = field_status)
  writeData(wb = wb, sheet = args$domain, startCol = 1, startRow = nrow(field_status) + 4, x = "Detailed Description of findings:")
  writeData(wb = wb, sheet = args$domain, startCol = 1, startRow = nrow(field_status) + 5, x = log_out)
  
  addStyle(wb = wb, sheet = args$domain, rows = 3:(nrow(field_status) + 2), cols = 1, style = centered_style )
  addStyle(wb = wb, sheet = args$domain, rows = 3:(nrow(field_status) + 2), cols = 3:7, style = centered_style, gridExpand = T )
  
  setColWidths(wb = wb, sheet = args$domain, cols = 3, widths = 16)
  setColWidths(wb = wb, sheet = args$domain, cols = 6:7, widths = 16)
  
  addStyle(wb = wb, sheet = args$domain, rows = 2, cols = 1:7, style = table_header )
  conditionalFormatting(wb = wb, sheet = args$domain, rows = 3:(nrow(field_status) + 2), cols = 5, type = "contains", rule = "PASS", style = posStyle )
  conditionalFormatting(wb = wb, sheet = args$domain, rows = 3:(nrow(field_status) + 2), cols = 5, type = "contains", rule = "FAIL", style = negStyle )
  conditionalFormatting(wb = wb, sheet = args$domain, rows = 3:(nrow(field_status) + 2), cols = 5, type = "contains", rule = "Warning", style = warnStyle )
  
  #if (args$verbose){
  # Hide the rows with system information
  hide_start <- nrow(field_status) + 5
  hide_end <- nrow(field_status) + 5 + 6 + length(args)
  groupRows(wb = wb, sheet = args$domain, rows = hide_start:hide_end, hidden = TRUE)  
  #}
  
  saveWorkbook(wb, file.path(args$outpath, excel_filename), overwrite = TRUE)
}

add_to_log(lvl = "info", func="main", message=paste0("Process began at ", init, " and finished at ", Sys.time()), add = F)
add_to_log(lvl = "info", func="main", message=paste0("Elapsed time: ", (proc.time() - timer)[['elapsed']]), add = F)
add_to_log(lvl = "info", func="main", message="Finished", add = F)

if (args$unittest){
  IsRegulatedChar(x = "YES", criteria <- c("YES" , "NO" , "UNKNOWN" , "PREFER NOT TO ANSWER"), case_sensitive = T) # T
  IsRegulatedChar(x = "yes", criteria <- c("YES" , "NO" , "UNKNOWN" , "PREFER NOT TO ANSWER"), case_sensitive = T) # FALSE
  IsRegulatedChar(x = "yes", criteria <- c("YES" , "NO" , "UNKNOWN" , "PREFER NOT TO ANSWER"), case_sensitive = F) # TRUE
  IsRegulatedChar(x = "Other, Acetominiphen", criteria <- c("YES" , "NO" , "UNKNOWN" , "PREFER NOT TO ANSWER"), case_sensitive = F) # FALSE
  IsRegulatedChar(x = "Other, Acetominiphen", criteria <- c("YES" , "NO" , "UNKNOWN" , "PREFER NOT TO ANSWER", "OTHER, (SPECIFY)"), case_sensitive = F) # FALSE
  IsRegulatedChar(x = "Other, Acetominiphen", criteria <- c("YES" , "NO" , "UNKNOWN" , "PREFER NOT TO ANSWER", "OTHER, [SPECIFY]"), case_sensitive = F) # TRUE
  IsRegulatedChar(x = "Other, Acetominiphen", criteria <- c("YES" , "NO" , "UNKNOWN" , "PREFER NOT TO ANSWER", "OTHER, [SPECIFY]"), case_sensitive = T) # FALSE
  IsRegulatedChar(x = "OTHER, Acetominiphen", criteria <- c("YES" , "NO" , "UNKNOWN" , "PREFER NOT TO ANSWER", "OTHER, [SPECIFY]"), case_sensitive = T) # TRUE
  IsRegulatedChar(x = "yes", criteria = "[FREE TEXT]", case_sensitive = F) # TRUE
  IsRegulatedChar(x = "yes", criteria = "[FREE TEXT]", case_sensitive = T) # TRUE
}
