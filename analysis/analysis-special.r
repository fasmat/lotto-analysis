winRatios <- get.WinRatios(data)

###########################################################

data.analysis <- winRatios[, c("ratio.win3zz", "data.ZZ")]
data.analysis <- data.analysis[complete.cases(data.analysis), ]

x <- 0:9
avg <- c()
sdev <- c()

for (i in 0:9) {
    avg <- c(avg, mean(data.analysis$ratio.win3zz[data.analysis$data.ZZ == i], na.rm = T))
    sdev <- c(sdev, sd(data.analysis$ratio.win3zz[data.analysis$data.ZZ == i], na.rm = T))
}

file <- file.path(outdir, "win3-special.png")
png(file, bg = "white", width = 600, height = 400)
plot(x, avg,
    xlim = c(-0.2, 9.5),
    # ylim=range(c(avg-sdev, avg+sdev)),
    ylim = c(0.7, 1.5),
    pch = 19,
    xlab = "Special number", ylab = "Pick frequency",
    main = "Pick frequency of special numbers"
)
axis(1, at = x, labels = x)
text(x + 0.3, avg, labels = round(avg, 2), cex = 0.7)
arrows(x, avg - sdev, x, avg + sdev, length = 0.05, angle = 90, code = 3)
dev.off()

###########################################################

data.analysis <- winRatios[, c("ratio.win4zz", "data.ZZ")]
data.analysis <- data.analysis[complete.cases(data.analysis), ]

x <- 0:9
avg <- c()
sdev <- c()

for (i in 0:9) {
    avg <- c(avg, mean(data.analysis$ratio.win4zz[data.analysis$data.ZZ == i], na.rm = T))
    sdev <- c(sdev, sd(data.analysis$ratio.win4zz[data.analysis$data.ZZ == i], na.rm = T))
}

file <- file.path(outdir, "win4-special.png")
png(file, bg = "white", width = 600, height = 400)
plot(x, avg,
    xlim = c(-0.2, 9.5),
    # ylim=range(c(avg-sdev, avg+sdev)),
    ylim = c(0.7, 1.5),
    pch = 19,
    xlab = "Special number", ylab = "Pick frequency",
    main = "Pick frequency of special numbers"
)
axis(1, at = x, labels = x)
text(x + 0.3, avg, labels = round(avg, 2), cex = 0.7)
arrows(x, avg - sdev, x, avg + sdev, length = 0.05, angle = 90, code = 3)
dev.off()