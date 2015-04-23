## creates two functions that will save the result of a matrix inverse calculation
##  allowing it to be recalled later to save computation


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## returns inverse of a matrix.  If inverse had previously been calcuated, it will return the previous result.
## Otherwise it calcuates the inverse, saves it, and returns

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(is.null(m)){
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setmean(m)
  m
}
