#######################################################################
# TSP - Traveling Salesperson Problem 
# Copyrigth (C) 2011 Michael Hahsler and Kurt Hornik
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.



## interface to the Concorde algorithm 
## (can only handle TSP and no neg. distances!)

tsp_concorde <- function(x, control = NULL, stdout = ""){
  
  ## get parameters
  clo         <- if(!is.null(control$clo))        control$clo         else ""
  precision   <- if(!is.null(control$precision))  control$precision   else 6
  exe         <- .find_exe(control$exe, "concorde")
  
  ## check x
  if(inherits(x, "TSP")){
    
    ## fix neg. values
    min_x <- min(x)
    if(min_x<0) x <- x - min_x
    
    ## get max (excluding) to check for possible integer overflows
    max_x <- max(x)
    if(n_of_cities(x) < 10){
      ## <10 cities: concorde can only handle max 2^15
      MAX <- 2^15
      if(max_x > MAX) stop("Concorde can only handle distances < 2^15 for less than 10 cities")
      
      prec <- floor(log10(MAX / max_x))
      if(prec < precision) {
        precision <- prec
        warning(paste("Concorde can only handle distances < 2^15 for",
          "less than 10 cities. Reducing precision to", 
          precision), immediate. = TRUE)
      }
    }else{
      ## regular constraint on integer is 2^31 - 1    
      MAX <- 2^31 - 1
      
      prec <- floor(log10(MAX / max_x / n_of_cities(x)))
      if(prec < precision) {
        precision <- prec
        warning(paste("Concorde can only handle distances < 2^31.",
          "Reducing precision for Concorde to", precision), immediate. = TRUE)
      }
    }
  }else if(inherits(x, "ETSP")) {
    ## nothing to do!
  }else stop("Concorde only handles TSP and ETSP.")
  
  
  ## get temp files and change working directory
  wd <- tempdir()
  dir <- getwd()
  setwd(wd)
  on.exit(setwd(dir))
  
  ### fix for Windows by Stephen Eick
  ##temp_file <- tempfile(tmpdir = wd) 
  temp_file <- basename(tempfile(tmpdir = wd)) 
  
  ## file name needs to be unique
  tmp_file_in  <- paste(temp_file, ".dat", sep = "")
  tmp_file_out <- paste(temp_file, ".sol", sep = "")
  
  write_TSPLIB(x, file = tmp_file_in, 
    precision = precision)
  
  ## change working directory
  
  ## do the call and read back result
  ## we do not check return values of concorde since they are not
  ## very consistent
  system2(exe, paste("-x", "-o", tmp_file_out , clo, tmp_file_in),  stdout = stdout)
  
  if(!file.access(tmp_file_out) == 0) 
    stop("Problems with reading Concorde's output.\nIs concorde properly installed?\nFor details see ? Concorde")
  ##else cat("Concorde done.\n")
  
  order <- scan(tmp_file_out, what = integer(0), quiet = TRUE)
  ## remove number of nodes and add one (result starts with 0)
  order <- order[-1] + 1L 
  
  ## tidy up
  unlink(c(tmp_file_in, tmp_file_out))
  
  order
}

## interface to the Concorde's Chained Lin-Kernighan algorithm 
## (can only handle TSP, handles neg. distances)

tsp_linkern <- function(x, control = NULL, stdout = ""){
  
  ## get parameters
  clo         <- if(!is.null(control$clo))        control$clo         else ""
  precision   <- if(!is.null(control$precision))  control$precision   else 6
  exe         <- .find_exe(control$exe, "linkern")
  verbatim    <- if(!is.null(control$verbatim))   control$verbatim    else FALSE 
  
  verbatim    <- if(!verbatim) "-Q" else "" 
  
  
  
  ## have to set -r for small instances <8
  if(n_of_cities(x) <=8) clo <- paste(clo, "-k", n_of_cities(x))
  
  ## check x
  if(inherits(x, "TSP")) {
    ## check for possible overflows
    max_x <- max(abs(x[is.finite(x)]))
    MAX <- 2^31 - 1
    
    prec <- floor(log10(MAX / max_x / n_of_cities(x)))
    if(prec < precision) {
      precision <- prec
      warning(paste("Linken can only handle distances < 2^31.",
        "Reducing precision to", precision), immediate. = TRUE)
    }
  }else if(inherits(x, "ETSP")) {
    ## nothing to do
  } else stop("Linkern only works for TSP and ETSP.")
  
  ## get temp files
  wd <- tempdir()
  temp_file <- tempfile(tmpdir = wd) 
  
  ## file name needs to be unique
  tmp_file_in  <- paste(temp_file, ".dat", sep = "")
  tmp_file_out <- paste(temp_file, ".sol", sep = "")
  
  ## prepare data (neg_inf = 0 so everything is > 0)
  write_TSPLIB(x, file = tmp_file_in, 
    precision = precision)
  
  ## change working directory
  dir <- getwd()
  setwd(wd)
  on.exit(setwd(dir))
  
  ## do the call and read back result
  ## we do not check return values of concorde since they are not
  ## very consistent
  system2(exe, paste(verbatim, "-o", tmp_file_out , clo, tmp_file_in),stdout = stdout)
  
  if(!file.access(tmp_file_out) == 0) 
    stop("Problems with reading linkern's output. Is linkern properly installed?")
  ##else cat("Concorde done.\n")
  
  order <- read.table(tmp_file_out)[,1]
  ## remove number of nodes and add one (result starts with 0)
  order <- order + as.integer(1) 
  
  ## tidy up
  unlink(c(tmp_file_in, tmp_file_out))
  
  order
}


## get help page
concorde_help <- function(exe = NULL) {
  system(paste(.find_exe(exe, "concorde"), ""))
}

linkern_help <- function(exe = NULL) {
  system(paste(.find_exe(exe, "linkern"), ""))
}

## path
concorde_path <- local({
  .path <- NULL
  function(path){
    if(missing(path)) .path else {
      .path <<- path
      if(!is.null(path)) {
        ex <- c(list.files(path, pattern = "concorde", 
          ignore.case = TRUE),
          list.files(path, pattern = "linkern", 
            ignore.case = TRUE))
        if(length(ex) < 1)
          warning(paste("no executable (concorde, linkern) found in", 
            path))
        cat("found:", ex, "\n")
      }
      invisible(.path)
      
    }
  }
})


## helper to find the concorde executable
.find_exe <- function(exe = NULL, prog) {
  ## if not specified
  if(is.null(exe)) {
    ## was the path set ?
    if(!is.null(concorde_path())) 
      exe <- paste(concorde_path(), .Platform$file.sep, prog, sep ="")
    ## no, so it must be in the systems execution path
    else exe <- prog
  }
  exe
}

