# specifying a non-default filename: cat joy-arch-plot.R | R --no-save --args /tmp/log.txt
filename <- "/tmp/log.txt"
co <- commandArgs()
args <- which(co=="--args")+1
if (length(args)>0) {
  filename <- co[args]
}
cat("plotting filename=",filename,"\n")

t <- read.table(filename)
# 3 columns for fitness parameters
fits <- 3
# mut_length_const is the number of parameters after the fitness parameters and before the joy-ops frequencies (see *mut-length-const*)
mut_length_const <- 6+3

postscript("/tmp/log.ps")
par(mfrow=c(3,2))
if (all(t$V2 <= 0)) {
  plot(-t$V2, type="b", log="y", ylim=range(c(-t$V1,-t$V2)), main="fitness")
  lines(-t$V1, type="b")
} else if (all(t$V2 >= 0)) {
  plot(t$V2, type="b", log="y", ylim=range(c(t$V1,t$V2)),main="fitness")
  lines(t$V1, type="b")
} else {
  plot(t$V2, type="b",main="fitness")
  lines(t$V1, type="b")
}
plot(t$V3, type="b", log="y",main="fitness stddev")

# plot mutate parameters
matplot(t[,seq(fits+1,fits+mut_length_const*2,2)], type="b", cex=0.25,main="mutate parameters")
matplot(t[,seq(fits+2,fits+mut_length_const*2,2)], type="b", cex=0.25,main="mutate parameters stddev")
# plot joy-ops frequencies
matplot(t[,seq(fits+mut_length_const*2+1,length(t),2)], type="b", cex=0.25,main="joy-ops freqs")
matplot(t[,seq(fits+mut_length_const*2+2,length(t),2)], type="b", cex=0.25,main="joy-ops freqs stddev")
dev.off()
