library(rmeta)
data(catheter)

exc <- c("Appavu", "Pemberton", "Logghe", "Bach(a)")
subs <- which(!(catheter$Name %in% exc))
amh <- meta.MH(n.trt, n.ctrl, col.trt, col.ctrl, data=catheter,
               names=Name, subset=subs)
a <- as.data.frame(amh[c("logOR","selogOR")])
names(a) <- c("est","se")
catheter <- cbind(name=catheter[subs,"Name"], a)

