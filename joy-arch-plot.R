t <- read.table("/tmp/log.txt")
#t <- read.table("/tmp/log2.txt")

postscript("/tmp/log.ps")
par(mfrow=c(3,2))
if (all(t$V1 <= 0)) {
	plot(-t$V1, type="b", log="y")
} else if (all(t$V1 >= 0)) {
	plot(t$V1, type="b", log="y")
} else {
	plot(t$V1, type="b")
}
plot(t$V2, type="b", log="y")
matplot(t[,seq(3,28,2)], type="b", cex=0.5)
matplot(t[,seq(4,28,2)], type="b", cex=0.5)
matplot(t[,seq(29,108,2)], type="b", cex=0.5)
matplot(t[,seq(30,108,2)], type="b", cex=0.5)
dev.off()
