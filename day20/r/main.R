expand <- function(m, infinity) {
    mplus <- array(infinity, dim=dim(m) + 4)
    d <- dim(m)
    mplus[3:(d[1] + 2), 3:(d[2] + 2)] <- m
    mplus
}

readFile <- function(fname) {
    readChar(fname, file.info(fname)$size)
}

processInput <- function(s) {
    lines <- unlist(strsplit(s, "\n"))
    dict <- unlist(strsplit(lines[1], NULL)) == '#'
    bits <- unlist(strsplit(lines[-1:-2], NULL)) == '#'

    ## image is a square matrix
    l <- sqrt(length(bits))
    image <- matrix(bits, l, l)

    list(dict=dict, image=image, infinity=FALSE)
}

enhance <- function(problem) {
    ## Make index array
    sz <- dim(problem$image) + 2
    indexes <- matrix(0, prod(sz) * 9, 2)
    ## one 3x3 block per position in the input
    indexes[,1] <- 1:3
    indexes[,2] <- rep(1:3, each=3)
    ## The position of each 3x3 block
    blocks <- matrix(0, prod(sz), 2)
    blocks[,1] <- 1:sz[1] - 1
    blocks[,2] <- rep(1:sz[2] - 1, each=sz[1])
    ## Blow that up and apply it to the indexes
    indexes[,1] <- indexes[,1] + rep(blocks[,1], each=9)
    indexes[,2] <- indexes[,2] + rep(blocks[,2], each=9)

    ## Do the index
    indexedValues <- expand(problem$image, problem$infinity)[indexes]
    indexedValues <- matrix(indexedValues, 9, length(indexedValues) / 9)

    ## reshape into columns of 9 and sum
    bits <- matrix(indexedValues, 9, length(indexedValues) / 9)
    values <- colSums(bits * rev(2 ^ (0:8)))
    ## index into the dict, taking into account 1-indexing, and that's
    ## our result
    problem$image <- matrix(problem$dict[c(values + 1)], sz)

    ## update infinity too
    if (problem$infinity) {
        problem$infinity <- problem$dict[sum(2 ^ (0:8)) + 1]
    } else {
        problem$infinity <- problem$dict[1]
    }

    problem
}

render <- function(grid) {
    sz = dim(grid)
    cgrid <- matrix('·', sz[1], sz[2])
    cgrid[grid] <- '#'
    output <- rep('', sz[1])
    for (i in 1:sz[1]) {
        output[i] <- paste(cgrid[,i], collapse="")
    }
    paste(output, collapse="\n")
}

no.dimnames <- function(a) {
  ## Remove all dimension names from an array for compact printing.
  d <- list()
  l <- 0
  for(i in dim(a)) {
    d[[l <- l + 1]] <- rep("", i)
  }
  dimnames(a) <- d
  a
}

sm <- processInput(readFile('../example.txt'))
sm$image <- matrix(c(TRUE), 1, 1)
ex <- processInput(readFile('../example.txt'))
inp <- processInput(readFile('../input.txt'))

ex <- enhance(ex)
ex <- enhance(ex)
sum(ex$image)
if (sum(ex$image) != 35) { stop() }

for (idx in 3:50) {
    ex <- enhance(ex)
}
sum(ex$image)
if (sum(ex$image) != 3351) { stop() }



inp <- enhance(inp)
inp <- enhance(inp)
sum(inp$image)
if (sum(inp$image) <= 5565) { stop() }
if (sum(inp$image) != 5583) { stop() }

for (idx in 3:50) {
    inp <- enhance(inp)
}
sum(inp$image)
if (sum(inp$image) != 19592) { stop() }
