## makeCacheMatrix creates an object, with 4 associated methods allowing to store and get the variable and its 
## inverse in the cache
## cacheSolve is a function which computes the inverse of the variable if it has not been done and stored in the cache already
## We need first to create an object with makeCacheMatrix applied on a matrix, then to use cacheSolve() on this object.


## makeCacheMatrix creates an object with 4 associated methods : 
## - get : gets the matrix
## - set : stores the matrix given as argument into the object
## - getsolve : gets the inverse of the matrix
## - setsolve : stores the inverse of the matrix given as argument into the object
## One must understand that the object created is not a matrix but an object which has 4 associated methods, with 2 of them
## allorwing to get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve takes the object created by makeCacheMatrix as argument.
## First it gets the inverse of the object thanks to the getsolve() method associated to the object.
## If it exists then the inverse s is return and the message "getting cached data" is printed.
## Otherwise, the functions takes the matrix from the object with the get() method, computes the inverse with the solve() function
## from the base R package, stores it in the object with the setsolve() method and returns the result.
## This way, next time the function cacheSolve is applied on the same object, it can return the result without computing it.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
