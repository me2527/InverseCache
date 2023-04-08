makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getinv <- function() inv
  setinv <- function(inverse) {
    inv <<- inverse
  }
  return(list(
    set = set,
    get = get,
    getinverse = getinv,
    setinverse = setinv
  ))
}
cacheSolve <- function(x,...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    return(inverse)
  }
  m <- solve(x$get())
  x$setinverse(m)
  #return(m)
}
I <- rbind(c(0,1,2),c(1,1,1),c(-4,2,1))
m <- makeCacheMatrix(I)
identical(m$get(),I)

m$getinverse()
cacheSolve(m)

m$getinverse()

m$set(rbind(sqrt(c(2,2)),c(-1,2)))

m$getinverse()
