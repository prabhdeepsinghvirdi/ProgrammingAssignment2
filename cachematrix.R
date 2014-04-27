## Matrix Inverse Cache
## The two functions check if an inverse of the matrix already exists in the Cache to 
## avoid the repetitive computation. If inverse of the given matrix is not present in the cache,
## then the set of functions calculate the Matrix Inverse and moves it to the cache. 

## The first function, `makeCacheMatrix` creates a special "Matrix", which is
#really a list containing a function to

#1.  set the value of the Matrix
#2.  get the value of the Matrix
#3.  set the value of the Inverse
#4.  get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInverse <- function(Inverse) m <<- Inverse
      getInverse <- function() m
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## The following function calculates the mean of the special "Matrix"
#created with the above function. However, it first checks to see if the
#Matrix Inverse has already been calculated. If so, it `get`s the Matrix Inverse from the
#cache and skips the computation. Otherwise, it calculates the Matrix Inverse of
#the data and sets the value of the Inverse in the cache via the `setInverse`
#function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getInverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...) ## Solve() returns the inverse of a Vector
      x$setInverse(m)
      m
}
