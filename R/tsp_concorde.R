# interface to the Concorde algorithm

tsp_concorde <- function(x, options = NULL){

    # get parameters
    clo         <- if(!is.null(options$clo))        options$clo         else ""
    precision   <- if(!is.null(options$precision))  options$precision   else 6
    exe         <- .find_concorde(options$exe)
    
    # check x
    if(!inherits(x, "TSP")) x <- TSP(x)
    
    # get temp files
    wd <- tempdir()
    temp_file <- tempfile(tmpdir = wd) 
    
    # file name needs to be unique
    tmp_file_in  <- paste(temp_file, ".dat", sep = "")
    tmp_file_out <- paste(temp_file, ".sol", sep = "")
    
    # prepare data
    write_TSPLIB(x, file = tmp_file_in, precision = precision)

    # change working directory
    dir <- getwd()
    setwd(wd)
    on.exit(setwd(dir))
    
    # do the call and read back result
    # we do not check return values of concorde since they are not
    # very consistent
    system(paste(exe, "-x", "-o", tmp_file_out , clo, tmp_file_in))
    
    if(!file.access(tmp_file_out) == 0) 
    stop("Problems with reading Concorde's output. Is Concorde properly installed?")
    
    order <- scan(tmp_file_out, what = integer(0), quiet = TRUE)
    # remove number of nodes and add one (result starts with 0)
    order <- order[-1] + as.integer(1) 

    # tidy up
    unlink(c(tmp_file_in, tmp_file_out))
    
    order
}

# get help page
tsp_concorde_help <- function(exe = NULL) {
      system(paste(.find_concorde(exe), ""))
}
  
# helper to find the concorde executable
.find_concorde <- function(exe = NULL) {
    # use environment variable?
    if(is.null(exe))    exe <- Sys.getenv("R_CONCORDE")
    # last resort (hopefully it is in the PATH)
    if(exe == "")       exe <- "concorde"
    
    exe
}


