## 
## Balaji Rajagopalan, Nov 2019
##
## Problem: Computing inversion of a matrix is expensive. Devise a method
## to avoid repeatedly computing the inverted matrix.
## 
## Approach: Wrap the matrix in a list that also stores its inversion. The list
## that wraps the matrix can be conceptually thought of as a "cached matrix".
## Higher level functions that need caching facility can use the cached matrix,
## instead of the regular matrix.
##
## makeCacheMatrix() function accepts a regular matrix as input, and creates 
## a list that wraps the given matrix, and a set of closure functions that 
## work on the given matrix. Also stored in the list is an inverted matrix, 
## if it has been computed.
##
## cacheSolve() function accepts a "cached matrix", created using makeCacheMatrix(),
## and computes the inverted matrix. Once computed, the inverted matrix is stored
## as a member of the list. Subsequent invocations  simply retrieve
## the inverted matrix from the list, rather than re-compute it.
##

## Given a matrix, return a "cached matrix", which can store the 
## inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    set_inverse <- function(invp) inv <<- invp
    get_inverse <- function() inv
    list(set = set, get = get, 
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## Given a "cached matrix", return its inverted matrix
## Inverted matrix is computed first call, and cached. Subsequent
## calls return the result from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inverse()
    if (!is.null(inv)) {
        return(inv)
    }
    m <- x$get()
    inv <- solve(m)
    x$set_inverse(inv)
    inv
}

