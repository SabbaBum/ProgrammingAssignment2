## Put comments here that give an overall description of what your functions do

##
## This function acts as a "constructor", which caches values (or you can say - allocates space)
## for 2 matrixes:
## 1) the normal, which could be passed as an argument during function call
##    or assigned later via <object>$doSetMatrix(...) call
## 2) the inverse, which shold be explicitly calculated via cacheSolve() function
##
## Also, it does define the methods of manipulating both matrices,
## which are then returned back (exported) as a list object
##
makeCacheMatrix <- function(varCachedMatrix = matrix()) {

    ## here we set initial values for varCached* - empty matrix() for direct one
    ## and NULL value for the inversed one
    varCachedInverse <- NULL

    ## NOTE: these vars are not accessible 'outside' this function context,
    ##       that is why we do need special 'mechanism' - functions below
    ##       to access and change them

    ## this function should be called to load the new source matrix
    ## (in this case we have to clear possible old cached matrix value)
    ## or if makeCacheMatrix was initially called without any parameters
    funcSetMatrix <- function(varTmpMatrix) {
        varCachedMatrix <<- varTmpMatrix
        varCachedInverse <<- NULL
    }

    ## function returns cached value of 'normal' matrix
    funcGetMatrix <- function()
        varCachedMatrix

    ## function sets cached value of 'inverse' matrix
    funcSetInverse <- function(varTmpMatrix)
        varCachedInverse <<- varTmpMatrix

    ## function returns cached value of 'reversed' matrix
    funcGetInverse <- function()
        varCachedInverse

    ## here we 'export' the list with function names that should be used
    ## to manipulate cached vars 'outside' of this current function context
    ## since the var names are not accessible
    ## NOTE: I intentially use external names as do* and internal as func*
    ##       just for the purpose that I can always know which is which :)
    list(
        doSetMatrix = funcSetMatrix,
        doGetMatrix = funcGetMatrix,
        doSetInverse = funcSetInverse,
        doGetInverse = funcGetInverse
    )
}


## This is a function that generated the inverse matrix (solves it against an identity matrix)
## and caches the value of this potentially time-consuming operation
## If it is called when we have a valid cache - it returnes the cached value and skips compute

cacheSolve <- function(varTmpMatrix, ...) {
    ## Let's see, if we do have inverse matrix already cached
    varTmpCache <- varTmpMatrix$doGetInverse()
    ## If it's so - print a debug/info message and return cached value
    if (!is.null(varTmpCache)) {
        message("getting cached data")
        return(varTmpCache)
    }
    ## If not - well, have to do all the hard work
    ## 1) pull the data
    varTmpData <- varTmpMatrix$doGetMatrix()
    ## 2) solve the matrix against and identity one (unless we provide anything in '...')
    ##    this is defalut if solve() gets just only one matrix value
    varTmpCache <- solve(varTmpData, ...)
    ## 3) let's write down (cache) the result
    varTmpMatrix$doSetInverse(varTmpCache)
    ## 4) and return it
    varTmpCache
}

##
## PLEASE STOP HERE: THIS IS NOT A PART OF ASSIGNMENT !!!
##
## THIS IS A PIECE OF CODE FOR TESTING ABOVE FUNCTIONS FOR MY CONVENIENCE ONLY :)
##

testAll <- function() {
    # generate some 3x3 matrix filled with random normal distribution values
    A <- matrix(rnorm(9), nrow = 3, ncol = 3)
    print(A)
    # init the constructor, check that we have empty matrix ready
    B <- makeCacheMatrix()
    print(B$doGetMatrix())
    # set the matrix we'll play with and check if we have clear reverse value
    B$doSetMatrix(A)
    print(B$doGetMatrix())
    print(B$doGetInverse())
    # solving matrix for 1st time - should go and compute
    cacheSolve(B)
    print(B$doGetInverse())
    # solving matrix for 2nd time - should see the message and get the cached result
    cacheSolve(B)
    print(B$doGetMatrix())
}
