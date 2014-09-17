## makeCacheMatrix prepares getters and setters for matrix Inversion
## It returns 4 functions get, set, getSol and putSOl as list
## for the 4 operations - getting and setting the matrix and inverted matrix

## Use get and set for initial matrix
## Use getSol and setSol for inverted matrix
makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
        x <<- y
        s <<- NULL
      }
      get <- function() x
      setSol <- function(sol) s <<- sol
      getSol <- function() s
      list(set = set, get = get,
           setSol = setSol,
           getSol = getSol)
}


## cacheSolve prepares the matrix and checks if inverted value already exists,
## if so get from cache or prepare an inversion and save onto the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      s <- x$getSol()
      if(!is.null(s)) {
        message("Retrieving cached data")
        return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setSol(s)
      s
}
