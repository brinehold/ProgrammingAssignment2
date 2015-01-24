## Put comments here that give an overall description of what your
## functions do

## Creates special matrix that is used to compute the inverse.

makeCacheMatrix <- function(x = matrix()) {
        mat<-NULL
        set<-function(y){
                x<<-y
                mat<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) mat<<- solve # sets inverse matrix
        getmatrix<-function() mat
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

## Function that checks to see if the inverse of a given matrix has already been
## calculated. Will return the already calculated inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat<-x$getmatrix()
        if(!is.null(mat)){
                message("getting cached data")
                return(mat) ## return inverse if found
        }
        matrix<-x$get()
        mat<-solve(matrix, ...)
        x$setmatrix(mat)
        mat ## return inverse if NOT found
}
