## As R is only working on the memory, we need to be careful about the usages 
## of memory very efficiently. Here these functions will help not to do the same 
## inverse operation on a matrix repeatedly. The use of these functions will 
## save processing time and memory as well. It will do the followings:
##	
##	1) process inverse one time and save in cache 
##	2) allow to use multiple time from the same result from the cache
##
##


## The makeCacheMatrix function will allow a user to create a instance of an  
## object (CacheMatrix) in cache with two properties to set and get the followings 
## for a given matrix as parameter:
##		a) allow to set and get the matrix in Cache
##		b) allow to set and get the inverse of the matrix in Cache
## 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(msolve) m <<- msolve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The cacheSolve function will allow the user to inverse the matrix 
## in the instance from makeCacheMatrix and set the property of inverse 
## of the matrix in cache.
##

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        d<-det(data)
        if (d>0) {
            m <- solve(data, ...)
		x$setsolve(m)
        	m
        }
        else {
		message("Determinant is zero. Invert is not possible...")
        }
}

