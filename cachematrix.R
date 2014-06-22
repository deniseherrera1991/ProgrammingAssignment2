## These functions will create a matrix object which can cache it's ##inverse and instead of re-working out the inverse will, once re-##prompted to calculate the inverse, paste the answer from the cache. ##This will hopefully save some time on very large matrix ##calculations.
## 1.	makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) { ##this sets the value of 											##the matrix
        m <- NULL
        set <- function(y) {
                x <<- y 	##the instruction to cache in an alternate environment
                m <<- NULL
        }
        get <- function() x	##this gets the value of the matric
        setsolve <- function(solve) m <<- solve ##this sets the inverse of the matrix
        getsolve <- function() m ##this gets the inverse of the matrix
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## 	2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## This matrix will get the inverse of the matrix from the cache if it already exists.

cacheSolve <- function(x = matrix(), ...) {
        m <- x$getsolve() 
        if(!is.null(m)) { ##if the matrix in the cache the function is now looking for it and it's inverse
                message("getting cached data") ##should this take a bit longer a message will appear
                return(m) ## and the matrix's inverse will be returned.
        }
        data <- x$get() ## should the matrix not be in the cache it will instruct to recalculate.
        m <- solve(matrix, ...)
        x$setsolve(m)
        m
}
##objects such as m, setsolve and getsolve are not strictly free variables but rather defined by the other function, thus enabling both functions to work together.