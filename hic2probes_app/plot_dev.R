setwd("/Users/phanstiel2/Research/Eric/hic2probes_app/hic2probes_app/")

data <- as.data.frame(read.delim("../hic2probes/output/filtered_probes.bed", header = F))
colnames(data) <- c("chr", "start", "stop", "shift", "res.fragment", "dir", "AT", "GC", "seq", "pass")

values <- sort(unlist(lapply(seq(1:nrow(data)), function(x) data$start[x]:data$stop[x])))

# col_func <- colorRampPalette(c("blue", "midnight blue", "purple", "maroon"))
# col_func(4)

cols <- c("green", "blue", "purple", "red")[factor(data$pass)]
#cols <- col_func(4)[factor(data$pass)]
cols <- adjustcolor(cols, alpha.f = 0.6)

plot(c(0,1), c(0,1), "n", xlim=c(min(data$start), max(data$stop)), xlab = "", frame.plot = F)
segments(data$start, data$GC, data$stop, data$GC, col = cols, lwd=5)
legend('topright', title = "Pass Number", legend = c(0, 1, 2, 3), fill = col_func(4))


hist(data$GC, breaks = 50, col = "grey", xlim = c(0, 1))


plot(data$start, data$shift, "n", frame.plot = F)
segments(data$start, data$shift, data$stop, data$shift, col = cols, lwd = 5)

## Custom histogram
custom_histogram <- function(data=data, breaks = breaks){
  ## Cut table into bins along with pass numbers
  bins <- table(cut(data$start, breaks = breaks, include.lowest = T), data$pass)
  bin_start <- as.numeric(gsub("^[:(:]|\\[|,.*", "", rownames(bins)))
  bin_stop <- as.numeric(gsub(".*,|\\]","", rownames(bins)))
  bdata <- as.data.frame(cbind(bin_start, bin_stop, bins))
  bdata$bins <- rowSums(bdata[,3:ncol(bdata)])
  bin_size <- bdata$bin_stop[1] - bdata$bin_start[1]
  
  ## Define color palette
  cols <- c("green", "blue", "purple", "red")
  cols <- adjustcolor(cols, alpha.f = 0.6)
  
  ## Plot stacked histogram barplot
  plot(c(0,1), c(0,1), "n",
       xlim = c(min(bdata$bin_start), max(bdata$bin_stop)),
       ylim = c(0, max(bdata$bins)),
       xlab = "genomic coordinates",
       ylab = "Number of Probes",
       main = "Histogram of Probe Coverage",
       sub = paste0("N= ", sum(bdata$bins), ", ",
                    "bin size= ", bin_size, "bp"),
       frame.plot = F)
  rect(bdata$bin_start, 0, bdata$bin_stop, bdata$bins, border = NA)
  rect(bdata$bin_start, 0, bdata$bin_stop, bdata$`0`, col = cols[1], border = NA)
  rect(bdata$bin_start, bdata$`0`, bdata$bin_stop, bdata$`0`+bdata$`1`, col = cols[2], border = NA)
  rect(bdata$bin_start, bdata$`0`+bdata$`1`, bdata$bin_stop, bdata$`0`+bdata$`1`+bdata$`2`, col = cols[3], border = NA)
  rect(bdata$bin_start, bdata$`0`+bdata$`1`+bdata$`2`, bdata$bin_stop, bdata$`0`+bdata$`1`+bdata$`2`+bdata$`3`, col = cols[4], border = NA)
  legend('topleft', title = "Pass Number", legend = c(0:3), fill = cols, bty = 'n', cex = 0.55, border = NA)
  
}

custom_histogram(data, 100)

## Its crazy that this kind of works
# barplot(t(table(cut(data$start, breaks = 100), data$pass)), col = c("green", "blue", "red", "purple"),
#         legend = rownames(t(table(cut(data$start, breaks = 100), data$pass))))


setwd("/Users/phanstiel2/Research/Eric/hic2probes_app/hic2probes_app")

## Load in restriction sites
res.sites <- read.delim("../hic2probes/output/fragments.bed", header = F)
sites <- unique(sort(c(res.sites[,2], res.sites[,3])))
sites <- sites + 133000000


start <- 133000000
end <- 133050000

plot(c(0,1), c(0,1), "n", xlim=c(start, end), xlab = "", frame.plot = F)
segments(data$start, data$GC, data$stop, data$GC, col = cols, lwd=5)

if (end - start <= 50000){
  segments(sites, -1, sites, 0.8)
}


