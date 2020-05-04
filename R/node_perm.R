#' Node permutation test
#'
#' This function creates a random or semi-random walk bounded by a maximum distance from the starting position.
#' @param mod generalized linear model
#' @param nperm number of shuffles
#' @param offset names of offset variable to shuffle with response if present
#' @param plot default TRUE, to plot the distribution of random betas
#' @keywords random walk, bounded
#' @export
#' @examples
#' node_perm()

###build function for node permutations


node_perm <- function(mod,
                      nperm = 1000,
                      offset = NA,
                      plot = TRUE) {
  #make matrix
  ncoef <- length(coef(mod)) - 1

  nmat <- matrix(ncol = ncoef, nrow = nperm)
  colnames(nmat) <- names(coef(mod))[-1]

  for (i in 1:nperm) {
    this_it <- mod$data
    new_order <- sample(1:nrow(this_it))
    this_it[, all.vars(mod$formula)[1]] <-
      this_it[new_order, all.vars(mod$formula)[1]]
    if (!is.na(offset)) {
      this_it[, offset] <- this_it[new_order, offset]
    }
    this_mod <- update(mod, data = this_it)
    nmat[i, ] <- coef(this_mod)[-1]

  }

  x <- numeric(ncoef)
  lci<-numeric(ncoef)
  uci<-numeric(ncoef)

  for (j in 1:ncoef) {
    x[j] <- ecdf(nmat[, j])(coef(mod)[j + 1])
    lci[j]<-quantile(nmat[,j], 0.025)
    uci[j]<-quantile(nmat[,j], 0.975)
  }

  names(x) <- colnames(nmat)



  if(plot){
    windows()
    par(mfrow = c(ceiling(ncoef / 3), 3))

        for (k in 1:ncoef) {
          hist(nmat[, k], main=colnames(nmat)[k],
               xlim=c(min(nmat[,k],coef(mod)[k + 1]), max(nmat[,k],coef(mod)[k + 1])),
               xlab=ecdf(nmat[, k])(coef(mod)[k + 1]))
          abline(v = coef(mod)[k + 1], col = "red", lwd=2)

        }
  }

  results<-data.frame(beta=coef(mod), lci=c(NA,lci), uci=c(NA,uci), quant=c(NA,x), pvalue=c(NA,x))
  results$pvalue<-ifelse(results$pvalue>0.5, 1-results$pvalue, results$pvalue)
  results$pvalue<-results$pvalue*2
  results<-round(results, 3)

  return(results)
}
