library(nlsr)

# a "self-starting" function which determines the starting values for each individual participant.
# from https://code.bioconductor.org/browse/xcms/blob/RELEASE_3_19/R/models.R
SSgauss <- selfStart(~ A*exp(-0.5*((x-C)/B)*((x-C)/B)),
            function(mCall, data, LHS, ...) {
                      xy <- sortedXyData(mCall[["x"]], LHS, data)
                      
                      len <- dim(xy)[1]
                      xyarea <- sum((xy[2:len,2]+xy[1:(len-1),2])*(xy[2:len,1]-xy[1:(len-1),1]))/2
                      maxpos <- which.max(xy[,2])
                      
                      A <- xy[maxpos,2]
                      B <- xyarea/(A*sqrt(2*pi))
                      C <- xy[maxpos,1]
                      
                      value <- c(A, B, C)
                      names(value) <- mCall[c("A", "B", "C")]
                      value
                    }, c("A", "B", "C")
                )

# gets the initial values using SSgauss, then puts the coefficients into a list.
iVals  <- function(y){
            fit <- getInitial(y ~ SSgauss(sec,A,B,C),data = data.frame(y, sec))
            list(A = fit["A"],
                 B = fit["B"],
                 C = fit["C"])}

# uses nlsr package's nlxb function, which hopes to give a minimal residual sum of squares.
# First, it sets the data frame as df, then runs nlxb, using the initial values from iVals 
# and setting the bounds for each coefficient (the upper C is the time recorded).
# Then it calculates the Sum of Squares Regression, R² = SSR/SST, Mean Squared Error,
# appending them to the list that nlxb() creates.
initxb <- function(y){
      df <- data.frame(y, sec)
      fit <- nlxb(y ~ A*exp(-0.5*((sec-C)/B)*((sec-C)/B)), 
           data = df,
           start = iVals (y),
           lower = c(A = -Inf,B = 0, C = 0),
           upper = c(A = Inf,B=Inf, C = length(sec)))
      s <- sum((fitted(fit, data = df$y) - mean(df$y))^2)
      r <- s/sum(s,fit$ssquares)
      m <- fit$ssquares/length(fit$resid)
      c(fit[['coefficients']],r.Squ = r,mse = m,SSqu = fit[['ssquares']],SSreg = s)
 }


