#### Preloads ####
library(latex2exp)
source(file = "1-Functions_OSB.R")
FigGen <- T # Set to TRUE or FALSE to save the figures as PDFs or plot them in RStudio

############ Figure 1: Changing the pinning point with N = 10 ############
#### Settings
H <- 1 # horizon
N <- c(10, 100, 500) # length for the partition  
a <- 1 # slope
b <- c(-5, 0, 5) # pulling level
v <- 1 # volatility
z <- 0 # pinning point
t_eval <- boundary_N1 <- sapply(1:length(b), function(i){
                                log(seq(exp(0), exp(H), l = N[i]))
                          })  # time line in finite horizon for the BM
#### Computations
boundary_N1 <- sapply(1:length(b), function(i){
  OUB_boundary_picard(z = z, a = a, b = b[i], v = v, 
                      t_eval = t_eval[[1]], H = H, errors = F)
}) 
boundary_N2 <- sapply(1:length(b), function(i){
  OUB_boundary_picard(z = z, a = a, b = b[i], v = v, 
                      t_eval = t_eval[[2]], H = H, errors = F)
}) 
boundary_N3 <- sapply(1:length(b), function(i){
  OUB_boundary_picard(z = z, a = a, b = b[i], v = v, 
                      t_eval = t_eval[[3]], H = H, errors = F)
}) 
#### Plot
color <- c("firebrick3", "deepskyblue3", "darkolivegreen3")

op <- par(no.readonly = TRUE) # Save the default settings

par(mar = c(4, 3.5, 1, 1), cex.axis = 2, cex.lab = 2,
    mgp = c(1.5, 1.5, 0), tcl = -0.6) # Setting for plotting in RStudio

if(FigGen == TRUE) { # Setting for saving the PDF image
  pdf(paste0("OSB_OUB_ChangePinning_N", N[1], ".pdf"), width = , height = )
  par(mar = c(4, 3.5, 1, 1), cex.axis = 2, cex.lab = 2,
      mgp = c(1.5, 1.5, 0), fin = c(6, 6.5), tcl = -0.6) # Setting for printing in RStudio
}

# ymax <- 1.05*max(boundary_N1, z) - 0.05*min(boundary_N1, z)
# ymin <- 1.05*min(boundary_N1, z) - 0.05*max(boundary_N1, z)

ymax <- 2; ymin <- 0

matplot(t_eval[[1]], boundary_N1, type = "l", lty = 1, xlab = "", ylab = "",
        ylim = c(ymin, ymax), col = color)
lines(c(0, T), c(z, z), lty = 3, col = "black")
mtext(side = 1, text = "Time", line = 4, cex = 2)
mtext(side = 2, text = "Boundary", line = 4, cex = 2)
legend("topright", inset = c(0.05, -0.05), bty = "n", title = "",
       legend = sapply(b, function(x) TeX(paste("$z$ =", x))),
       col = color, lty = 1, cex = 2, xpd = TRUE)

if (FigGen == TRUE) dev.off()

if(FigGen == TRUE) { # Setting for saving the PDF image
  pdf(paste0("OSB_OUB_ChangePinning_N", N[2], ".pdf"), width = , height = )
  par(mar = c(4, 3.5, 1, 1), cex.axis = 2, cex.lab = 2,
      mgp = c(1.5, 1.5, 0), fin = c(6, 6.5), tcl = -0.6) # Setting for printing in RStudio
}

# ymax <- 1.05*max(boundary_n2, z) - 0.05*min(boundary_n2, z)
# ymin <- 1.05*min(boundary_n2, z) - 0.05*max(boundary_n2, z)

ymax <- 2; ymin <- 0

matplot(t_eval[[2]], boundary_N2, type = "l", lty = 1, xlab = "", ylab = "",
        ylim = c(ymin, ymax), col = color)
lines(c(0, T), c(z, z), lty = 3, col = "black")
mtext(side = 1, text = "Time", line = 4, cex = 2)
mtext(side = 2, text = "Boundary", line = 4, cex = 2)
legend("topright", inset = c(0.05, -0.05), bty = "n", title = "",
       legend = sapply(b, function(x) TeX(paste("$z$ =", x))),
       col = color, lty = 1, cex = 2, xpd = TRUE)

if (FigGen == TRUE) dev.off()

if(FigGen == TRUE) { # Setting for saving the PDF image
  pdf(paste0("OSB_OUB_ChangePinning_N", N[3], ".pdf"), width = , height = )
  par(mar = c(4, 3.5, 1, 1), cex.axis = 2, cex.lab = 2,
      mgp = c(1.5, 1.5, 0), fin = c(6, 6.5), tcl = -0.6) # Setting for printing in RStudio
}

# ymax <- 1.05*max(boundary_N3, z) - 0.05*min(boundary_N3, z)
# ymin <- 1.05*min(boundary_N3, z) - 0.05*max(boundary_N3, z)

ymax <- 2; ymin <- 0

matplot(t_eval[[3]], boundary_N3, type = "l", lty = 1, xlab = "", ylab = "",
        ylim = c(ymin, ymax), col = color)
lines(c(0, T), c(z, z), lty = 3, col = "black")
mtext(side = 1, text = "Time", line = 4, cex = 2)
mtext(side = 2, text = "Boundary", line = 4, cex = 2)
legend("topright", inset = c(0.05, -0.05), bty = "n", title = "",
       legend = sapply(b, function(x) TeX(paste("$z$ =", x))),
       col = color, lty = 1, cex = 2, xpd = TRUE)

if (FigGen == TRUE) dev.off()

############ Figure 2: Changing the slope with z - b = 0 ############
#### Settings
H <- 1 # horizon
N <- 500 # length for the partition  
a <- c(0.1, 1, 5) # slope
b <- 0 # pulling level
v <- 1 # volatility
z <- 0 # pinning point
t_eval <- log(seq(exp(0), exp(H), l = N)) # time line in finite horizon for the BM
#### Computations
boundary <- sapply(1:length(a), function(i){
  OUB_boundary_picard(z = z, a = a[i], b = b, v = v, t_eval = t_eval, H = H, errors = F)
}) 
#### Plot
color <- c("firebrick3", "deepskyblue3", "darkolivegreen3")
ymax <- 1.05*max(boundary, z) - 0.05*min(boundary, z)
ymin <- 1.05*min(boundary, z) - 0.05*max(boundary, z)

par(mar=c(3.5, 3, 1, 1), cex.axis = 2, cex.lab = 2,
    mgp = c(1.5, 1.5, 0), tcl = -0.6) # Setting for plotting in RStudio

if(FigGen == TRUE) { # Setting for saving the PDF image
  pdf("OSB_OU_ChangeSlope_PinningEqualPullingLevel.pdf", width = , height = )
  par(mar = c(4, 3.5, 1, 1), cex.axis = 2, cex.lab = 2,
      mgp = c(1.5, 1.5, 0), fin = c(6, 6.5), tcl = -0.6) # Setting for printing in RStudio
}

matplot(t_eval, boundary, type = "l", lty = 1, xlab = "", ylab = "",
        ylim = c(ymin, ymax), col = color)
lines(c(0, T), c(z, z), lty = 3, col = "black")
lines(t_eval, z + 0.8399*sqrt(H - t_eval), lty = 2, col = "black", lwd = 2)
mtext(side = 1, text = "Time", line = 4, cex = 2)
mtext(side = 2, text = "Boundary", line = 4, cex = 2)
legend("topright", inset = c(0.05, -0.05), bty = "n", title = "",
       legend = sapply(a, function(x) TeX(paste("$\\alpha$ =", x))),
       col = color, lty = 1, cex = 2, xpd = TRUE)

if (FigGen == TRUE) dev.off()

############ Figure 3: Changing the slope with z - b = -5 ############
#### Settings
H <- 1 # horizon
N <- 500 # length for the partition  
a <- c(0.1, 1, 5) # slope
b <- 5 # pulling level
v <- 1 # volatility
z <- 0 # pinning point
t_eval <- log(seq(exp(0), exp(H), l = N)) # time line in finite horizon for the BM
#### Computations
boundary <- sapply(1:length(a), function(i){
  OUB_boundary_picard(tol = 1e-4, z = z, a = a[i], b = b, v = v, t_eval = t_eval, H = H, errors = F)
}) 
#### Plot
color <- c("firebrick3", "deepskyblue3", "darkolivegreen3")
ymax <- 1.05*max(boundary, z) - 0.05*min(boundary, z)
ymin <- 1.05*min(boundary, z) - 0.05*max(boundary, z)

par(mar = c(4, 3.5, 1, 1), cex.axis = 2, cex.lab = 2,
    mgp = c(1.5, 1.5, 0), tcl = -0.6) # Setting for plotting in RStudio

if(FigGen == TRUE) { # Setting for saving the PDF image
  pdf("OSB_OU_ChangeSlope_PinningLowerPullingLevel.pdf", width = , height = )
  par(mar = c(4, 3.5, 1, 1), cex.axis = 2, cex.lab = 2,
      mgp = c(1.5, 1.5, 0), fin = c(6, 6.5), tcl = -0.6) # Setting for printing in RStudio
}

matplot(t_eval, boundary, type = "l", lty = 1, xlab = "", ylab = "",
        ylim = c(ymin, ymax), col = color)
lines(c(0, T), c(z, z), lty = 3, col = "black")
lines(t_eval, z + 0.8399*sqrt(H - t_eval), lty = 2, col = "black", lwd = 2)
mtext(side = 1, text = "Time", line = 4, cex = 2)
mtext(side = 2, text = "Boundary", line = 4, cex = 2)
legend("topright", inset = c(0.05, -0.05), bty = "n", title = "",
       legend = sapply(a, function(x) TeX(paste("$\\alpha$ =", x))),
       col = color, lty = 1, cex = 2, xpd = TRUE)

if (FigGen == TRUE) dev.off()

############ Figure 4: Changing the slope with z - b = 5 ############
#### Settings
H <- 1 # horizon
N <- 500 # length for the partition  
a <- c(0.1, 1, 5) # slope
b <- -5 # pulling level
v <- 1 # volatility
z <- 0 # pinning point
t_eval <- log(seq(exp(0), exp(H), l = N)) # time line in finite horizon for the BM
#### Computations
boundary <- sapply(1:length(a), function(i){
  OUB_boundary_picard(z = z, a = a[i], b = b, v = v, t_eval = t_eval, H = H, errors = F)
}) 
#### Plot
color <- c("firebrick3", "deepskyblue3", "darkolivegreen3")
ymax <- 1.05*max(boundary, z) - 0.05*min(boundary, z)
ymin <- 1.05*min(boundary, z) - 0.05*max(boundary, z)

par(mar = c(4, 3.5, 1, 1), cex.axis = 2, cex.lab = 2,
    mgp = c(1.5, 1.5, 0), tcl = -0.6) # Setting for plotting in RStudio

if(FigGen == TRUE) { # Setting for saving the PDF image
  pdf("OSB_OU_ChangeSlope_PinningGreaterPullingLevel.pdf", width = , height = )
  par(mar=c(3.5, 3, 1, 1), cex.axis = 2, cex.lab = 2,
      mgp = c(1.5, 1.5, 0), fin = c(6, 6.5), tcl = -0.6) # Setting for printing in RStudio
}

matplot(t_eval, boundary, type = "l", lty = 1, xlab = "", ylab = "",
        ylim = c(ymin, ymax), col = color)
lines(c(0, T), c(z, z), lty = 3, col = "black")
lines(t_eval, z + 0.8399*sqrt(H - t_eval), lty = 2, col = "black", lwd = 2)
mtext(side = 1, text = "Time", line = 4, cex = 2)
mtext(side = 2, text = "Boundary", line = 4, cex = 2)
legend("topright", inset = c(0.05, -0.05), bty = "n", title = "",
       legend = sapply(a, function(x) TeX(paste("$\\alpha$ =", x))),
       col = color, lty = 1, cex = 2, xpd = TRUE)

if (FigGen == TRUE) dev.off()

############ Figure 5: Changing the volatility with z - b = 0 ############
#### Settings
H <- 1 # horizon
N <- 500 # length for the partition  
a <- 1 # slope
b <- 0 # pulling level
v <- c(1, 5, 10) # volatility
z <- 0 # pinning point
t_eval <- log(seq(exp(0), exp(H), l = N)) # time line in finite horizon for the BM
#### Computations
boundary <- sapply(1:length(v), function(i){
  OUB_boundary_picard(z = z, a = a, b = b, v = v[i], t_eval = t_eval, H = H, errors = F)
}) 
#### Plot
color <- c("firebrick3", "deepskyblue3", "darkolivegreen3")
ymax <- 1.05*max(boundary, z) - 0.05*min(boundary, z)
ymin <- 1.05*min(boundary, z) - 0.05*max(boundary, z)

par(mar = c(4, 3.5, 1, 1), cex.axis = 2, cex.lab = 2,
    mgp = c(1.5, 1.5, 0), tcl = -0.6) # Setting for plotting in RStudio

if(FigGen == TRUE) { # Setting for saving the PDF image
  pdf("OSB_OU_ChangeVolatility_PinningEqualPullingLevel.pdf", width = , height = )
  par(mar = c(4, 3.5, 1, 1), cex.axis = 2, cex.lab = 2,
      mgp = c(1.5, 1.5, 0), fin = c(6, 6.5), tcl = -0.6) # Setting for printing in RStudio
}

matplot(t_eval, boundary, type = "l", lty = 1, xlab = "", ylab = "",
        ylim = c(ymin, ymax), col = color)
lines(c(0, T), c(z, z), lty = 3, col = "black")
mtext(side = 1, text = "Time", line = 4, cex = 2)
mtext(side = 2, text = "Boundary", line = 4, cex = 2)
legend("topright", inset = c(0.05, -0.05), bty = "n", title = "",
       legend = sapply(v, function(x) TeX(paste("$\\gamma$ =", x))),
       col = color, lty = 1, cex = 2, xpd = TRUE)

if (FigGen == TRUE) dev.off()

############ Figure 6: Changing the volatility with z - b = -5 ############
#### Settings
H <- 1 # horizon
N <- 500 # length for the partition  
a <- 1 # slope
b <- 5 # pulling level
v <- c(1, 5, 10) # volatility
z <- 0 # pinning point
t_eval <- log(seq(exp(0), exp(H), l = N)) # time line in finite horizon for the BM
#### Computations
boundary <- sapply(1:length(v), function(i){
  OUB_boundary_picard(z = z, a = a, b = b, v = v[i], t_eval = t_eval, H = H, errors = F)
}) 
#### Plot
color <- c("firebrick3", "deepskyblue3", "darkolivegreen3")
ymax <- 1.05*max(boundary, z) - 0.05*min(boundary, z)
ymin <- 1.05*min(boundary, z) - 0.05*max(boundary, z)

par(mar = c(4, 3.5, 1, 1), cex.axis = 2, cex.lab = 2,
    mgp = c(1.5, 1.5, 0), tcl = -0.6) # Setting for plotting in RStudio

if(FigGen == TRUE) { # Setting for saving the PDF image
  pdf("OSB_OU_ChangeVolatility_PinningLowerPullingLevel.pdf", width = , height = )
  par(mar=c(3.5, 3, 1, 1), cex.axis = 2, cex.lab = 2,
      mgp = c(1.5, 1.5, 0), fin = c(6, 6.5), tcl = -0.6) # Setting for printing in RStudio
}

matplot(t_eval, boundary, type = "l", lty = 1, xlab = "", ylab = "",
        ylim = c(ymin, ymax), col = color)
lines(c(0, T), c(z, z), lty = 3, col = "black")
mtext(side = 1, text = "Time", line = 4, cex = 2)
mtext(side = 2, text = "Boundary", line = 4, cex = 2)
legend("topright", inset = c(0.05, -0.05), bty = "n", title = "",
       legend = sapply(v, function(x) TeX(paste("$\\gamma$ =", x))),
       col = color, lty = 1, cex = 2, xpd = TRUE)

if (FigGen == TRUE) dev.off()

############ Figure 7: Changing the volatility with z - b = 5 ############
#### Settings
H <- 1 # horizon
N <- 500 # length for the partition  
a <- 1 # slope
b <- -5 # pulling level
v <- c(1, 5, 10) # volatility
z <- 0 # pinning point
t_eval <- log(seq(exp(0), exp(H), l = N)) # time line in finite horizon for the BM
#### Computations
boundary <- sapply(1:length(v), function(i){
  OUB_boundary_picard(z = z, a = a, b = b, v = v[i], t_eval = t_eval, H = H, errors = F)
}) 
#### Plot
color <- c("firebrick3", "deepskyblue3", "darkolivegreen3")
ymax <- 1.05*max(boundary, z) - 0.05*min(boundary, z)
ymin <- 1.05*min(boundary, z) - 0.05*max(boundary, z)

par(mar = c(4, 3.5, 1, 1), cex.axis = 2, cex.lab = 2,
    mgp = c(1.5, 1.5, 0), tcl = -0.6) # Setting for plotting in RStudio

if(FigGen == TRUE) { # Setting for saving the PDF image
  pdf("OSB_OU_ChangeVolatility_PinningGreaterPullingLevel.pdf", width = , height = )
  par(mar = c(4, 3.5, 1, 1), cex.axis = 2, cex.lab = 2,
      mgp = c(1.5, 1.5, 0), fin = c(6, 6.5), tcl = -0.6) # Setting for printing in RStudio
}

matplot(t_eval, boundary, type = "l", lty = 1, xlab = "", ylab = "",
        ylim = c(ymin, ymax), col = color)
lines(c(0, T), c(z, z), lty = 3, col = "black")
mtext(side = 1, text = "Time", line = 4, cex = 2)
mtext(side = 2, text = "Boundary", line = 4, cex = 2)
legend("topright", inset = c(0.05, -0.05), bty = "n", title = "",
       legend = sapply(v, function(x) TeX(paste("$\\gamma$ =", x))),
       col = color, lty = 1, cex = 2, xpd = TRUE)

if (FigGen == TRUE) dev.off()

par(op) # Reset the default settings

