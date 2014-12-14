# shamelessly copied from http://www.theresearchkitchen.com/archives/738
#
#
#

# Generate a binomial lattice
# for a given up, down, start value and number of steps
genlattice <- function(X0=100, u=1.1, d=.75, N=5) {
  X <- c()
  X[1] <- X0
  count <- 2
  
  for (i in 1:N) {
    for (j in 0:i) {
      X[count] <- X0 * u^j * d^(i-j)
      count <- count + 1
    }
  }
  return(X)
}

#
#


#create a function to graph a binomial lattice given a tool like graphviz

#


function(S, labels=FALSE) {
  shape <- ifelse(labels == TRUE, "plaintext", "point")
  
  cat("digraph G {", "\n", sep="")
  cat("node[shape=",shape,", samehead, sametail];","\n", sep="")
  cat("rankdir=LR;","\n")
  
  cat("edge[arrowhead=none];","\n")
  
  # Create a dot node for each element in the lattice
  for (i in 1:length(S)) {
    cat("node", i, "[label=\"", S[i], "\"];", "\n", sep="")
  }
  
  # The number of levels in a binomial lattice of length N
  # is `$\frac{\sqrt{8N+1}-1}{2}$`
  L <- ((sqrt(8*length(S)+1)-1)/2 - 1)
  
  k<-1
  for (i in 1:L) {
    tabs <- rep("\t",i-1)
    j <- i
    while(j>0) {
      cat("node",k,"->","node",(k+i),";\n",sep="")
      cat("node",k,"->","node",(k+i+1),";\n",sep="")
      k <- k + 1
      j <- j - 1
    }
  }
  
  cat("}", sep="")
}

# output a dot script to the screen. We can capture this script and save it to a file

x<-capture.output(dotlattice(genlattice(N=8, u=1.1, d=0.9)))
cat(x, file="lattice1.dot")

#to add labels to the lattice vertices, we can add the labels attribute

x<-capture.output(dotlattice(genlattice(N=8, u=1.1, d=0.9), labels=TRUE))
cat(x, file="/tmp/lattice1.dot")







