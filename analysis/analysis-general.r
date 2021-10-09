library(minpack.lm)
library(lattice)

winRatios <- get.WinRatios(data)

###########################################################

exponents <- apply(data, 1, getExponents, correctDrawn = 3)
data.analysis <- data.frame(winRatios$ratio.win3, t(exponents))
data.analysis <- data.analysis[complete.cases(data.analysis), ]

colnames(data.analysis) <- c(
    "ratio",
    "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10",
    "r11", "r12", "r13", "r14", "r15", "r16", "r17", "r18", "r19", "r20",
    "r21", "r22", "r23", "r24", "r25", "r26", "r27", "r28", "r29", "r30",
    "r31", "r32", "r33", "r34", "r35", "r36", "r37", "r38", "r39", "r40",
    "r41", "r42", "r43", "r44", "r45", "r46", "r47", "r48", "r49"
)

model <- nlsLM(ratio ~ x1^r1 * x2^r2 * x3^r3 * x4^r4 * x5^r5 * x6^r6 * x7^r7 * x8^r8 * x9^r9 *
    x10^r10 * x11^r11 * x12^r12 * x13^r13 * x14^r14 * x15^r15 * x16^r16 * x17^r17 * x18^r18 * x19^r19 *
    x20^r20 * x21^r21 * x22^r22 * x23^r23 * x24^r24 * x25^r25 * x26^r26 * x27^r27 * x28^r28 * x29^r29 *
    x30^r30 * x31^r31 * x32^r32 * x33^r33 * x34^r34 * x35^r35 * x36^r36 * x37^r37 * x38^r38 * x39^r39 *
    x40^r40 * x41^r41 * x42^r42 * x43^r43 * x44^r44 * x45^r45 * x46^r46 * x47^r47 * x48^r48 * x49^r49,
data = data.analysis,
start = c(
    x1 = 1, x2 = 1, x3 = 1, x4 = 1, x5 = 1, x6 = 1, x7 = 1, x8 = 1, x9 = 1,
    x10 = 1, x11 = 1, x12 = 1, x13 = 1, x14 = 1, x15 = 1, x16 = 1, x17 = 1, x18 = 1, x19 = 1,
    x20 = 1, x21 = 1, x22 = 1, x23 = 1, x24 = 1, x25 = 1, x26 = 1, x27 = 1, x28 = 1, x29 = 1,
    x30 = 1, x31 = 1, x32 = 1, x33 = 1, x34 = 1, x35 = 1, x36 = 1, x37 = 1, x38 = 1, x39 = 1,
    x40 = 1, x41 = 1, x42 = 1, x43 = 1, x44 = 1, x45 = 1, x46 = 1, x47 = 1, x48 = 1, x49 = 1
)
)

model.sum <- summary(model)
x <- 1:49
avg <- model.sum$coefficients[, 1]
sdev <- model.sum$coefficients[, 2]

file <- file.path(outdir, "win3-general.png")
png(file, bg = "white", width = 2000, height = 400)
plot(x, avg,
    xlim = c(-0.2, 49.5),
    ylim = c(0.7, 1.5),
    # ylim=range(c(avg-sdev, avg+sdev)),
    pch = 19,
    xlab = "Lotto number", ylab = "Pick frequency",
    main = "Pick frequency of lotto numbers"
)
axis(1, at = x, labels = x)
text(x + 0.5, avg, labels = round(avg, 2), cex = 0.7)
arrows(x, avg - sdev, x, avg + sdev, length = 0.05, angle = 90, code = 3)
dev.off()

# index least played numbers
best <- match(head(sort(avg), 6), avg)
best <- sort(best)

best.zz <- match(head(sort(avg.3zz),1), avg.3zz)

file <- file.path(outdir, "win3-numbers.png")
png(file, bg = "black", width = 600, height = 100)
par(mar=c(1,1,1,1))
plot.new()
plot.window(0:1, 0:1)
text(.5,.5, paste(c(best, " ", best.zz - 1), collapse=" "), cex=5, col=rgb(.9,.9,.9,.7))
dev.off()

# calculate expected return for best numbers
ret <- round(0.5 / mean(avg[best], avg.3zz[best.zz]), 2)

file <- file.path(outdir, "win3-expected-return.png")
png(file, bg = "black", width = 300, height = 100)
par(mar=c(1,1,1,1))
plot.new()
plot.window(0:1, 0:1)
text(.5,.5, paste(c("€", ret), collapse=" "), cex=5, col=rgb(.9,.9,.9,.7))
dev.off()

###########################################################

heatmap <- data.frame(expand.grid(x = seq(1, 7), y = seq(7, 1)),
    value = avg,
    labels = seq(1, 49)
)

file <- file.path(outdir, "win3-general-heat.png")
png(file, bg = "white", width = 800, height = 800)
print(levelplot(value ~ x + y, heatmap,
    col.regions = hcl.colors(100, "Temps"),
    title = "Lottery ticket heatmap",
    xlab = "", ylab = "",
    panel = function(...) {
        arg <- list(...)
        panel.levelplot(...)
        panel.text(arg$x, arg$y, arg$subscripts)
    }
))
dev.off()

###########################################################

exponents <- apply(data, 1, getExponents, correctDrawn = 4)
data.analysis <- data.frame(winRatios$ratio.win4, t(exponents))
data.analysis <- data.analysis[complete.cases(data.analysis), ]

colnames(data.analysis) <- c(
    "ratio",
    "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10",
    "r11", "r12", "r13", "r14", "r15", "r16", "r17", "r18", "r19", "r20",
    "r21", "r22", "r23", "r24", "r25", "r26", "r27", "r28", "r29", "r30",
    "r31", "r32", "r33", "r34", "r35", "r36", "r37", "r38", "r39", "r40",
    "r41", "r42", "r43", "r44", "r45", "r46", "r47", "r48", "r49"
)

model <- nlsLM(ratio ~ x1^r1 * x2^r2 * x3^r3 * x4^r4 * x5^r5 * x6^r6 * x7^r7 * x8^r8 * x9^r9 *
    x10^r10 * x11^r11 * x12^r12 * x13^r13 * x14^r14 * x15^r15 * x16^r16 * x17^r17 * x18^r18 * x19^r19 *
    x20^r20 * x21^r21 * x22^r22 * x23^r23 * x24^r24 * x25^r25 * x26^r26 * x27^r27 * x28^r28 * x29^r29 *
    x30^r30 * x31^r31 * x32^r32 * x33^r33 * x34^r34 * x35^r35 * x36^r36 * x37^r37 * x38^r38 * x39^r39 *
    x40^r40 * x41^r41 * x42^r42 * x43^r43 * x44^r44 * x45^r45 * x46^r46 * x47^r47 * x48^r48 * x49^r49,
data = data.analysis,
start = c(
    x1 = 1, x2 = 1, x3 = 1, x4 = 1, x5 = 1, x6 = 1, x7 = 1, x8 = 1, x9 = 1,
    x10 = 1, x11 = 1, x12 = 1, x13 = 1, x14 = 1, x15 = 1, x16 = 1, x17 = 1, x18 = 1, x19 = 1,
    x20 = 1, x21 = 1, x22 = 1, x23 = 1, x24 = 1, x25 = 1, x26 = 1, x27 = 1, x28 = 1, x29 = 1,
    x30 = 1, x31 = 1, x32 = 1, x33 = 1, x34 = 1, x35 = 1, x36 = 1, x37 = 1, x38 = 1, x39 = 1,
    x40 = 1, x41 = 1, x42 = 1, x43 = 1, x44 = 1, x45 = 1, x46 = 1, x47 = 1, x48 = 1, x49 = 1
)
)

model.sum <- summary(model)
x <- 1:49
avg <- model.sum$coefficients[, 1]
sdev <- model.sum$coefficients[, 2]

file <- file.path(outdir, "win4-general.png")
png(file, bg = "white", width = 2000, height = 400)
plot(x, avg,
    xlim = c(-0.2, 49.5),
    ylim = c(0.7, 1.5),
    # ylim=range(c(avg-sdev, avg+sdev)),
    pch = 19,
    xlab = "Lotto number", ylab = "Pick frequency",
    main = "Pick frequency of lotto numbers"
)
axis(1, at = x, labels = x)
text(x + 0.5, avg, labels = round(avg, 2), cex = 0.7)
arrows(x, avg - sdev, x, avg + sdev, length = 0.05, angle = 90, code = 3)
dev.off()

# index least played numbers
best <- match(head(sort(avg), 6), avg)
best <- sort(best)

best.zz <- match(head(sort(avg.4zz),1), avg.4zz)

file <- file.path(outdir, "win4-numbers.png")
png(file, bg = "black", width = 600, height = 100)
par(mar=c(1,1,1,1))
plot.new()
plot.window(0:1, 0:1)
text(.5,.5, paste(c(best, " ", best.zz - 1), collapse=" "), cex=5, col=rgb(.9,.9,.9,.7))
dev.off()

# calculate expected return for best numbers
ret <- round(0.5 / mean(avg[best], avg.4zz[best.zz]), 2)

file <- file.path(outdir, "win4-expected-return.png")
png(file, bg = "black", width = 300, height = 100)
par(mar=c(1,1,1,1))
plot.new()
plot.window(0:1, 0:1)
text(.5,.5, paste(c("€", ret), collapse=" "), cex=5, col=rgb(.9,.9,.9,.7))
dev.off()

###########################################################

heatmap <- data.frame(expand.grid(x = seq(1, 7), y = seq(7, 1)),
    value = avg,
    labels = seq(1, 49)
)

file <- file.path(outdir, "win4-general-heat.png")
png(file, bg = "white", width = 800, height = 800)
print(levelplot(value ~ x + y, heatmap,
    col.regions = hcl.colors(100, "Temps"),
    title = "Lottery ticket heatmap",
    xlab = "", ylab = "",
    panel = function(...) {
        arg <- list(...)
        panel.levelplot(...)
        panel.text(arg$x, arg$y, arg$subscripts)
    }
))
dev.off()