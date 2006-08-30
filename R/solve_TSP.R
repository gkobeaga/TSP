solve_TSP <- function(x, method = NULL, options = NULL, ...) {

    # check parameters
    if(!inherits(x, "TSP")) x <- TSP(x)
       
    # methods
    methods <- c(
        "farthest_insertion",      # standard
        "nearest_insertion",
        "greedy",
        "concorde"
    )

    if(is.null(method)) methodNr <- 1
    else methodNr <- pmatch(tolower(method), tolower(methods))
    if(is.na(methodNr)) stop (paste("Unknown method:",sQuote(method)))


    # work horses
    if(methodNr == 1) {
        order <- tsp_insertion(x, nearest = FALSE, options = options)
    }else if(methodNr == 2) {
        order <- tsp_insertion(x, nearest = TRUE, options = options)
    }else if(methodNr == 3) {
        order <- tsp_greedy(x, options = options)
    }else if(methodNr == 4) {
        order <- tsp_concorde(x, options = options)
    }

    if(!is.integer(order)) order <- as.integer(order)
    class(order) <- c("TOUR", class(order))
    attr(order, "method") <- methods[methodNr]
    attr(order, "tour_length") <- tour_length(x, order)
    return(order)
}
