test.file<-muc.tower.total.admin.2.2.days.file<-read.table('~/Desktop/SL/MUCTowerAdmin2Within2DaysWSelf.txt', sep = '|', header = FALSE)

tower.loc.ids<-read.csv('~/Desktop/SL/Tower_Locations_expanded.csv', header = T)

admin.2.names<-unique(tower.loc.ids[,c('district', 'admin2')])

plot(test.file$V3/104)

plot(test.file$V1, test.file$V2, cex = log(test.file$V3)/10, xaxt = 'n', yaxt = 'n')
axis(1, at = admin.2.names[,2], labels = admin.2.names[,1], las = 2, cex.axis = 0.6)
axis(2, at = admin.2.names[,2], labels = admin.2.names[,1], cex.axis = 0.6, las = 2)

mm<-sum(test.file[which(test.file$V2 == 41),3])
test.file[which(test.file$V1 == 41 & test.file$V2 == 41), ]

NN.districts<-dim(admin.2.names)[1]
total.from.vec<-rep(NA, NN.districts)
total.to.vec<-rep(NA, NN.districts)
total.within.vec<-rep(NA, NN.districts)
for(mm in 1:NN.districts){
  d.number<-admin.2.names[mm,2]
  total.from<-sum(test.file[which(test.file$V1 == d.number),3])
  total.to<-sum(test.file[which(test.file$V2 == d.number),3])
  total.within<-test.file[which(test.file$V1 == d.number & test.file$V2 == d.number),3]
  total.from.vec[mm] = total.from - total.within
  total.to.vec[mm] = total.to - total.within
  total.within.vec[mm] = total.within}

total.from.vec/104

par(mfrow=c(1,1))
plot(admin.2.names[,2], total.from.vec/(total.within.vec+total.from.vec), col = 'white')
text(admin.2.names[,2], total.from.vec/(total.within.vec+total.from.vec), labels = admin.2.names[,1], cex = 0.6)

numb.loc.visited<-read.csv('~/Desktop/SL/HistNumbLocVisited.txt', sep = '|', header = F)
numb.records<-read.csv('~/Desktop/SL/HistNumbRecords.txt', sep = '|', header = F)
numb.admin.2.visited<-read.csv('~/Desktop/SL/HistNumbLocVisitAdmin2.txt', sep = '|', header = F)
numb.admin.3.visited<-read.csv('~/Desktop/SL/HistNumbLocVisitAdmin3.txt', sep = '|', header = F)




par(mfrow=c(2,2))
plot(numb.loc.visited$V1, numb.loc.visited$V2/sum(numb.loc.visited$V2, na.rm=T), xlab = 'Number of Locations Visited', ylab = 'Percentage')
plot(numb.records$V1, numb.records$V2/sum(numb.records$V2, na.rm=T), xlab = 'Number of Records', ylab = 'Percentage')

3990055/sum(numb.loc.visited$V2, na.rm=T)
3740888/sum(numb.records$V2, na.rm=T)

admin.2.mvt.3.days<-read.table('~/Desktop/SL/MvtBtwnAdmin2Within3Days.txt', sep = '\t', header = F)
admin.2.mvt.2.days<-read.table('~/Desktop/SL/MvtBtwnAdmin2Within2Days.txt', sep = '|', header = F)
admin.2.mvt.1.days<-read.table('~/Desktop/SL/MvtBtwnAdmin2Within1Days.txt', sep = '|', header = F)

test.file<-read.table('~/Desktop/SL/MUCTowerAdmin2Within2DaysWSelf.txt', sep = '|', header = F)

colSums(admin.2.mvt.1.days[,3:107], na.rm=T)

ratio.values<-rep(NA, length(admin.2.names$district))
for(mm in 1:length(admin.2.names$district)){
  d1<-test.file[which(test.file$V1 == admin.2.names$admin2[mm]),]
  origin.name = admin.2.names$district[mm]
  numb.within<-d1[which(d1$V1 == d1$V2),]
  total<-sum(d1$V3, na.rm=T)
  ratio<-(total-numb.within$V3)/total
  ratio.values[mm] = ratio}

par(mfrow=c(1,1))
plot(ratio.values*100, xaxt = 'n', xlab = '', ylab = 'Perc Traveling')
text(seq(1,length(admin.2.names$district)), par('usr')[3], labels = admin.2.names$district, srt = 45, adj = c(1.1,1.1), xpd = T, cex = 0.9)

(20/500000)*0.14


5601232/5920096

ebola.cases.admin<-c(21,22,24,41)
admin.2.mvt.ebola<-admin.2.mvt[which(is.element(admin.2.mvt$V1,ebola.cases.admin)),]

quantile(apply(admin.2.mvt.3.days[,3:107], 1, mean), c(0.025, 0.5, 0.975))
quantile(apply(admin.2.mvt.2.days[,3:107], 1, mean), c(0.025, 0.5, 0.975))
quantile(apply(admin.2.mvt.1.days[,3:107], 1, mean), c(0.025, 0.5, 0.975))

mean(apply(admin.2.mvt.3.days[,3:107], 1, mean))
mean(apply(admin.2.mvt.2.days[,3:107], 1, mean))
mean(apply(admin.2.mvt.1.days[,3:107], 1, mean))

par(mfrow=c(2,2))
plot(colSums(admin.2.mvt.3.days[,3:107]), type = 'l', ylab = 'Total Trips', main = 'Record within 3 days')
plot(colSums(admin.2.mvt.2.days[,3:107]), type = 'l', ylab = 'Total Trips', main = 'Record within 2 days')
plot(colSums(admin.2.mvt.1.days[,3:107]), type = 'l', ylab = 'Total Trips', main = 'Record within 1 day')

par(mfrow=c(4,4))
for(mm in 1:length(admin.2.names$district)){
  d1.from<-admin.2.mvt.1.days[which(admin.2.mvt.1.days$V1 == admin.2.names$admin2[mm]),]
  d1.to<-admin.2.mvt.1.days[which(admin.2.mvt.1.days$V2 == admin.2.names$admin2[mm]),]
  plot(colSums(d1.from[,3:107]), type = 'l', col = 'blue', lwd = 2, main = admin.2.names$district[mm], ylab = 'Numb of Trips')
  lines(colSums(d1.to[,3:107]), type = 'l', col = 'red', lwd = 2)}

setwd('~/Desktop/SL/')
col.vec.list<-ifelse(is.element(admin.2.names$admin2, ebola.cases.admin), 'red', 'black')
par(mfrow=c(4,4))
for(mm in 1:length(admin.2.names$district)){
  d1.from<-admin.2.mvt.1.days[which(admin.2.mvt.1.days$V1 == admin.2.names$admin2[mm]),]
  jpeg(file = paste(paste('MvtFromDistrict', as.character(admin.2.names$district[mm]), sep = ''), '.jpeg', sep = ''), units = 'in', res = 300, 11.5, 8.27)
  par(mfrow=c(4,4))
  plot(colSums(d1.from[,3:107]), type = 'l', col = 'blue', lwd = 2, main = admin.2.names$district[mm], ylab = 'Numb of Trips')
  for(yy in 1:dim(d1.from)[1]){
    dest.name = admin.2.names$district[which(d1.from$V2[yy] == admin.2.names$admin2)]
    plot(t(d1.from[yy,3:107]), main = dest.name, type = 'l', lwd = 2, ylab = 'Travel To', col = col.vec.list[which(d1.from$V2[yy] == admin.2.names$admin2)])}
  dev.off()}

par(mfrow=c(3,3))
for(mm in 1:length(admin.2.names$district)){
  d1.from<-admin.2.mvt.1.days[which(admin.2.mvt.1.days$V1 == admin.2.names$admin2[mm]),]
#  jpeg(file = paste(paste('BarplotMvtFromDistrict', as.character(admin.2.names$district[mm]), sep = ''), '.jpeg', sep = ''), units = 'in', res = 300, 11.5, 8.27)
 # par(mfrow=c(1,1))
  cc.vec<-ifelse(is.element(d1.from$V2, ebola.cases.admin), 'red', 'grey')
  barplot(as.matrix(d1.from[,3:107]), col = cc.vec, main = admin.2.names$district[mm])
  #dev.off()
}
## BarplotMvtFromDistrict_1.pdf




























