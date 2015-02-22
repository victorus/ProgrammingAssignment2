## Put comments here that give an overall description of what your
## functions do

#Inverting a matrix is an expensive operation. Therefore, it is useful to keep the value of an inverted matrix in a cache, so the inverse is calculated at most once and whenever we need this inverse it can be accessed from cache.

#To achieve it we need two functions: 
#
#   makeCacheMatrix(x): a function creates a list of 4 functions that allow accessing the original matrix x and its inverse mi (initially set to NULL).
 
#   cacheSolve(X): a function which, when applied to the output from makeCacheMatrix(x)  returns the inverse of x, either directly from cache (when mi is not NULL) or first calculating the inverse and storing it in cache.

#Example usage:
#
#Let us create an invertable matrix of size 1000x1000:

#	set.seed(7)	
#	m<-rnorm(1000*1000) 
#	dim(m)<-c(1000,1000)
#	
#This matrix is invertible (we checked it, and setting the seed to 7 guarantees reproducibility!), but let us check it again:

#	a<-solve(m)

# Now create a cached version of m (after sourcing the file chagematrix.R!):

#	M<-makeCacheMatrix(m)

# and find it inverse:

#	b<-cacheSolve(M)

#that should take about 2 seconds).
# Try once again:

#	c<-caseSolve(M)

#This time we should get the result instantly (just a copy from cache) - you can check it with system.time(c<-caseSolve(M)).
#Finally, we can check that all 3 results are the same: mean(a==b) and mean(a==c) return 1.



## makeCacheMatrix <- function(x = matrix()) : creates a list of 4 functions that allow accessing the original matrix x and its inverse mi (initially set to NULL).

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) mi <<- inverse
        getinverse <- function() mi
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve(X): when applied to the output from makeCacheMatrix(x) returns the inverse of x, either calculating it with help of solve() (when called for the first time) or fetching it from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		mi <- x$getinverse()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        data <- x$get()
        mi <- solve(data, ...)
        x$setinverse(mi)
        mi
}
