ctl1 = c(42.04779889306376, 41.05848510512429, 40.09244820198673, 39.14914051781167, 38.22802727241324, 37.32858626808132, 36.45030759353713)
atl1 = c(121.0447165290353, 104.9309896405462, 90.96235591830477, 78.85325605478855, 68.35614499723839, 59.25643141022491, 51.36809080758646)
tsb1 = c(-78.99691763597153, -63.8725045354219, -50.86990771631802, -39.70411553697686, -30.12811772482515, -21.92784514214358, -14.91778321404933)

ctl7 = c(31.93062415352836, 31.17935042305469, 30.44575289632155, 29.72941568206853, 29.0299326742551, 28.34690732183095, 37.79712714345849)
atl7 = c(63.80221342161337, 55.30872877034106, 47.94591463428568, 41.56325377977107, 36.03026614339174, 31.23384144182351, 84.31842997764008)
tsb7 = c(-31.87158926808501, -24.12937834728636, -17.50016173796413, -11.83383809770254, -7.000333469136637, -2.886934119992562, -46.52130283418159)

ctl.we = c(32.33060548044023, 31.94637387753784, 31.57118259618665, 31.20481893286843, 30.84707518862, 33.93288230005692, 37.53429340853915)
atl.we = c(66.06528912586026, 59.40049268781135, 53.62292794933302, 48.61448476317021, 44.27277605293128, 59.94487136134578, 76.85871693323429)
tsb.we = c(-33.73468364542004, -27.45411881027352, -22.05174535314637, -17.40966583030177, -13.42570086431128, -26.01198906128886, -39.32442352469514)

ctl.fa = c(33.55407777452358, 34.3880605395448, 35.20242109674398, 35.9505644968798, 35.52821799062809, 36.3157525660524, 37.08475778121394)
atl.fa = c(72.98763833885083, 72.45679554814632, 71.99661966464285, 71.33145916073605, 64.23186330787144, 64.8666076786058, 65.41685354558628)
tsb.fa = c(-39.43356056432725, -38.06873500860151, -36.79419856789887, -35.38089466385624, -28.70364531724335, -28.55085511255341, -28.33209576437234)

ctl = as.data.frame(cbind(ctl1,ctl7,ctl.we,ctl.fa))
atl = as.data.frame(cbind(atl1,atl7,atl.we,atl.fa))
tsb = as.data.frame(cbind(tsb1,tsb7,tsb.we,tsb.fa))

ma = function(x,n=3){filter(x,rep(1/n,n), sides=2)}

pdf("performance_boxplots.pdf")
par(mfrow=c(3,1))
	boxplot(list("CTL[Monday]" = ctl$ctl1, "CTL[Sunday]" = ctl$ctl7, "CTL[Weekend]" = ctl$ctl.we, "CTL[Fatigue]" = ctl$ctl.fa), ylab= "CTL", main="CTL")
	boxplot(list("ATL[Monday]" = atl$atl1, "ATL[Sunday]" = atl$atl7, "ATL[Weekend]" = atl$atl.we, "ATL[Fatigue]" = atl$atl.fa), ylab= "ATL", main="ATL")
	boxplot(list("TSB[Monday]" = tsb$tsb1, "TSB[Sunday]" = tsb$tsb7, "TSB[Weekend]" = tsb$tsb.we, "TSB[Fatigue]" = tsb$tsb.fa), ylab= "TSB", main="TSB")
dev.off()

pdf("combined_plots.pdf", width=10)
	plot(ctl$ctl1, ylim=c(min(ctl),max(ctl)), type="l", lwd=3, col="blue", main="CTL", xlab="Day", ylab="CTL")
	lines(ctl$ctl7, lty=2, lwd=3, col="blue")
	lines(ctl$ctl.we, lty=3, lwd=3, col="blue")
	lines(ctl$ctl.fa, lty=4, lwd=3, col="blue")
	legend("topright", lty=c(1,2,3,4), lwd=3, col="blue", legend=c("Monday","Sunday","Weekend","Fatigue"))

	plot(atl$atl1, ylim=c(min(atl),max(atl)), type="l", lwd=3, col="red", main="ATL", xlab="Day", ylab="ATL")
	lines(atl$atl7, lty=2, lwd=3, col="red")
	lines(atl$atl.we, lty=3, lwd=3, col="red")
	lines(atl$atl.fa, lty=4, lwd=3, col="red")
	legend("topright", lty=c(1,2,3,4), lwd=3, col="red", legend=c("Monday","Sunday","Weekend","Fatigue"))

	plot(tsb$tsb1, ylim=c(min(tsb),max(tsb)), type="l", lwd=3, col="orange", main="TSB", xlab="Day", ylab="TSB")
	lines(tsb$tsb7, lty=2, lwd=3, col="orange")
	lines(tsb$tsb.we, lty=3, lwd=3, col="orange")
	lines(tsb$tsb.fa, lty=4, lwd=3, col="orange")
	legend("topleft", lty=c(1,2,3,4), lwd=3, col="orange", legend=c("Monday","Sunday","Weekend","Fatigue"))
dev.off()