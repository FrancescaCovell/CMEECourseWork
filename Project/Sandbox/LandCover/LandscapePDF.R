library(plot.matrix)

pdf(file = "/home/frcovell/Project/sandbox/LandCover/10PerCover.pdf",   # The directory you want to save the file in
    width = 4, 
    height = 4)
Land10<-read.table("/home/frcovell/Project/Data/Fragments/AmalgamationLandscape1_0.1.txt")
LandScape10 <- as.matrix(Land10)
colnames(LandScape10)<-c(1:11)
plot(LandScape10, col=c('white', 'green'), 
     main="10% Forest Cover",
     ylab="",
     xlab="")
dev.off()

pdf(file = "/home/frcovell/Project/sandbox/LandCover/30PerCover.pdf",   # The directory you want to save the file in
    width = 4, 
    height = 4)
Land30<-read.table("/home/frcovell/Project/Data/Fragments/AmalgamationLandscape1_0.3.txt")
LandScape30 <- as.matrix(Land30)
colnames(LandScape30)<-c(1:11)
plot(LandScape30, col=c('white', 'green'), 
     main="30% Forest Cover",
     ylab="",
     xlab="")
dev.off()

pdf(file = "/home/frcovell/Project/sandbox/LandCover/50PerCover.pdf",   # The directory you want to save the file in
    width = 4, 
    height = 4)
Land50<-read.table("/home/frcovell/Project/Data/Fragments/AmalgamationLandscape1_0.5.txt")
LandScape50 <- as.matrix(Land50)
colnames(LandScape50)<-c(1:11)
plot(LandScape50, col=c('white', 'green'), 
     main="50% Forest Cover",
     ylab="",
     xlab="")
dev.off()

pdf(file = "/home/frcovell/Project/sandbox/LandCover/70PerCover.pdf",   # The directory you want to save the file in
    width = 4, 
    height = 4)
Land70<-read.table("/home/frcovell/Project/Data/Fragments/AmalgamationLandscape1_0.7.txt")
LandScape70 <- as.matrix(Land70)
colnames(LandScape70)<-c(1:11)
plot(LandScape70, col=c('white', 'green'), 
     main="70% Forest Cover",
     ylab="",
     xlab="")
dev.off()

pdf(file = "/home/frcovell/Project/sandbox/LandCover/90PerCover.pdf",   # The directory you want to save the file in
    width = 4, 
    height = 4)
Land90<-read.table("/home/frcovell/Project/Data/Fragments/AmalgamationLandscape1_0.9.txt")
LandScape90 <- as.matrix(Land90)
colnames(LandScape90)<-c(1:11)
plot(LandScape90, col=c('white', 'green'), 
     main="90% Forest Cover",
     ylab="",
     xlab="")
dev.off()