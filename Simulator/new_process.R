library(plyr)
library(doMC)
library(ggplot2)
registerDoMC(cores=detectCores())

# command line argument, if present, indicates the results folder
args <- commandArgs(trailingOnly = T)
if (length(args) != 0) {
    res.folder <- args[1]
    #get the second folder to compare result
    if (length(args) == 3){
      res.folder_result <- args[3]
      res.folder_cs     <- args[2]
    }
} else {
    res.folder <- './'
}



# possible packet states
PKT_RECEIVING = 0
PKT_RECEIVED = 1
PKT_CORRUPTED = 2
PKT_GENERATED = 3
PKT_QUEUE_DROPPED = 4

# determine whether a string contains a parsable number"
is.number <- function(string) {
    if (length(grep("^[[:digit:]]*$", string)) == 1)
        return (T)
    else
        return (F)
}

# gets the list of files with a certain prefix and suffix in a folder
get.data.files <- function(folder, suffix=".csv") {
    if (strsplit(suffix, '')[[1]][1] == '.')
        suffix <- paste('\\', suffix, sep='')
    return(list.files(folder, pattern=paste('.*', suffix, sep='')))
}

# splits the name of an output file by _ and extracts the values of simulation parameters
get.params <- function(filename, fields) {
    p <- strsplit(gsub(".csv", "", basename(filename)), "_")[[1]]
    #to add a column, we need to have something in the dataframe, so we add a
    #fake column which we remove at the end
    d <- data.frame(todelete=1)
    for (f in 1:length(fields)) {
        v <- p[f]
        if (is.number(v))
            d[[fields[[f]]]] <- as.numeric(v)
        else
            d[[fields[[f]]]] <- v
    }
    d$todelete <- NULL
    return (d)
}

# computes the offered load
compute.offered.load <- function(d, data.rate, sim.time) {
    # keep generation events only
    d <- subset(d, event == PKT_GENERATED)
    offered.load <- ddply(d, c("src", "lambda"), function(x) {
        return(data.frame(ol=(sum(x$size * 8) / sim.time) / (1024**2)))
    }, .parallel=T)
    return(offered.load)
}

# computes the queue drop rate: dropped packets / generated packets
compute.drop.rate <- function(d, group=F) {
    fields <- c('lambda')
    if (!group)
        fields <- c('src', fields)
    drop.rate <- ddply(d, fields, function(x) {
        all.packets <- subset(x, event == PKT_GENERATED)
        lost.packets <- subset(x, event == PKT_QUEUE_DROPPED)
        return(data.frame(dr=nrow(lost.packets)/nrow(all.packets)))
    }, .parallel=T)
    return(drop.rate)
}

# computes collision rate: corrupter / (received + corrupted)
compute.collision.rate <- function(d, group=F) {
    fields <- c('lambda')
    if (!group)
        fields <- c('dst', fields)
    collision.rate <- ddply(d, fields, function(x) {
        all.packets <- subset(x, event == PKT_RECEIVED | event == PKT_CORRUPTED)
        lost.packets <- subset(all.packets, event == PKT_CORRUPTED)
        return(data.frame(cr=nrow(lost.packets)/nrow(all.packets)))
    }, .parallel=T)
    return(collision.rate)
}

# compute throughput: total bits received / simulation time
compute.throughput <- function(d, data.rate, sim.time, group=F) {
    fields <- c('lambda')
    if (!group)
        fields <- c('dst', fields)
    throughput <- ddply(d, fields, function(x) {
        received.packets <- subset(x, event == PKT_RECEIVED)
        return(data.frame(tr=sum(received.packets$size*8)/sim.time/(1024**2)))
    }, .parallel=T)
    return(throughput)
}

# total offered load in bits per second
offered.load <- function(lambda, n.nodes, packet.size=(1460+32)/2) {
    lambda*n.nodes*packet.size*8/1024/1024
}


#create a function 
process =function(folder) {

    # if there is no aggregated file, load all csv files into a single one
    aggregated.file <- paste(folder, 'alld.Rdata', sep='/')
    if (!file.exists(aggregated.file)) {
        alld <- data.frame()
        # find all csv in current folder
        data.files <- get.data.files(folder, '.csv')
        for (f in data.files) {
            full.path <- paste(folder, f, sep='/')
            print(full.path)
            pars <- get.params(full.path, c('prefix', 'lambda', 'seed'))
            d <- read.csv(full.path)
            d <- cbind(d, pars)
            alld <- rbind(d, alld)
        }
        save(alld, file=aggregated.file)
    } else {
        # otherwise simply load the aggregated file
        load(aggregated.file)
    }
    
    # get simulation time and number of nodes from the simulation data
    sim.time <- max(alld$time)
    n.nodes <- length(unique(alld$src))
    
    # compute the statistics
    cr <- compute.collision.rate(alld)
    cr$ol <- offered.load(cr$lambda, n.nodes=n.nodes)
    dr <- compute.drop.rate(alld)
    dr$ol <- offered.load(dr$lambda, n.nodes=n.nodes)
    tr <- compute.throughput(alld, 8e6, sim.time)
    tr$ol <- offered.load(tr$lambda, n.nodes=n.nodes)

    # and plot the results
    div <- 3
    p <- ggplot(tr, aes(x=ol, y=tr, color=factor(dst))) +
         geom_line() +
         geom_point() +
         xlab('total offered load (Mbps)') +
         ylab('throughput at receiver (Mbps)') +
         labs(color="receiver node") +
         ylim(c(0, 8.5))  +
         xlim(c(0, 90))
    ggsave(paste(folder, '/thr_', n.nodes, '.pdf', sep=''), width=16/div, height=9/div)
    print(p)
    
    #Create the throughput mean dataset
    mean_tr = aggregate(tr$tr, by=list(tr$ol), FUN=mean)
    mean_p <- ggplot(mean_tr, aes(x=Group.1, y=x)) +
      geom_line() +
      geom_point() +
      xlab('total offered load (Mbps)') +
      ylab('throughput at receiver (Mbps)') +
      labs(color="receiver node") +
      ylim(c(0, 8.5)) + 
      xlim(c(0, 90))
    print(p)
    ggplot(mean_tr, aes(x=Group.1, y=x)) +
      geom_line() +
      geom_point() +
      scale_linetype_manual(name="Window Size", values = c(1,5,3)) +
      scale_color_brewer(name="Window Size", palette = "Set1") +
      xlab('total offered load (Mbps)') +
      ylab('throughput at receiver (Mbps)') +
      ylim(c(0, 8.5))  +
      xlim(c(0, 90))
    ggsave(paste(folder, 'MEANthr.pdf', sep='/'), width=16/div, height=9/div)
    
    
    pcr <- ggplot(cr, aes(x=ol, y=cr, color=factor(dst))) +
           geom_line() +
           geom_point() +
           xlab('total offered load (Mbps)') +
           ylab('packet collision rate at receiver') +
           labs(color="receiver node") +
           ylim(c(0, 1))+
           xlim(c(0, 90))
    ggsave(paste(folder, '/pcr_', n.nodes, '.pdf', sep=''), width=16/div, height=9/div)
    print(pcr)
    
    
    #Create the collision rate mean dataset
    mean_pcr = aggregate(cr$cr, by=list(tr$ol), FUN=mean)
    plot_pcr <- ggplot(mean_pcr, aes(x=Group.1, y=x)) +
      geom_line() +
      geom_point() +
      xlab('total offered load (Mbps)') +
      ylab('packet collision rate at receiver') +
      labs(color="receiver node") +
      ylim(c(0, 1))+
      xlim(c(0, 90))
    ggsave(paste(folder, '/MEANpcr_', n.nodes, '.pdf', sep=''), width=16/div, height=9/div)
    print(plot_pcr)
    
    
    pdr <- ggplot(dr, aes(x=ol, y=dr, color=factor(src))) +
           geom_line() +
           geom_point() +
           xlab('total offered load (Mbps)') +
           ylab('packet drop rate at sender') +
           labs(color="sender node") +
           ylim(c(0, 1))+
           xlim(c(0, 90))

    ggsave(paste(folder, '/pdr_', n.nodes, '.pdf', sep=''), width=16/div, height=9/div)
    print(pdr)
    
    #Create the packet drop rate mean dataset
    mean_pdr = aggregate(dr$dr, by=list(tr$ol), FUN=mean)
    plot_mean_pdr <- ggplot(mean_pdr, aes(x=Group.1, y=x)) +
      geom_line() +
      geom_point() +
      xlab('total offered load (Mbps)') +
      ylab('packet drop rate at sender') +
      labs(color="sender node") +
      ylim(c(0, 1))+
      xlim(c(0, 90))
    ggsave(paste(folder, '/MEANpdr_', n.nodes, '.pdf', sep=''), width=16/div, height=9/div)
    print(plot_mean_pdr)
    
    #return 
    data.frame(mean_tr,mean_pcr,mean_pdr)
    
}

print(res.folder)
print(res.folder_cs)
print(res.folder_result)
#res.folder     = '/Users/sestari/Google\ Drive/SimulationPE/NEW/P2/python-simulator'
#res.folder_cs = '/Users/sestari/Google\ Drive/SimulationPE/NEW/P2/NewSimulator'
#res.folder_result = '/Users/sestari/Google\ Drive/SimulationPE/NEW/P2/result'

print("Base project")
dataset_1 = process(res.folder)

if (length(args) == 3) {
  
  print("New project")
  dataset_2 =process(res.folder_cs)
  
  print("Compare projects")
 
   #throughput
  mt_1 = dataset_1[1:2]
  mt_2 = dataset_2[1:2]
  mt_12 <- data.frame(mt_1$Group.1,mt_1$x,mt_2$x)

  mt_new = ggplot(mt_12, aes(mt_12$mt_1.Group.1)) +
      geom_line(aes(y=mt_12$mt_1.x,colour="no" )) +
      geom_line(aes(y=mt_12$mt_2.x,colour="yes") )  +
      xlab('total offered load (Mbps)') +
      ylab('Mean throughput at receiver (Mbps)') +
       labs(color="Carrier Sensing") + scale_colour_manual(values=c("red","blue"))+
      ylim(c(0, 8.5))  +
      xlim(c(0, 90))
  print(mt_new)
  div <- 3
  ggsave(paste(res.folder_result, 'ALLthr.pdf', sep='/'), width=16/div, height=9/div)
  
  
  #packet collision rate
  pcr_1 = dataset_1[3:4]
  pcr_2 = dataset_2[3:4]
  pcr_12 <- data.frame(pcr_1$Group.1.1,pcr_1$x.1,pcr_2$x.1)
  
  pcr_new = ggplot(pcr_12, aes(pcr_12$pcr_1.Group.1.1)) +
       geom_line(aes(y=pcr_12$pcr_1.x.1,colour="no" )) +
       geom_line(aes(y=pcr_12$pcr_2.x.1,colour="yes") )  +
       xlab('total offered load (Mbps)') +
       ylab('Mean packet collision rate at receiver') +
       labs(color="Carrier Sensing") + scale_colour_manual(values=c("red","blue"))+
      ylim(c(0, 1))+
      xlim(c(0, 90))
  ggsave(paste(res.folder_result, '/ALLpcr_',  '.pdf', sep=''), width=16/div, height=9/div)
  print(pcr_new)
  
  #packet drop rate
  pdr_1 = dataset_1[5:6]
  pdr_2 = dataset_2[5:6]
  pdr_12 <- data.frame(pdr_1$Group.1.2,pdr_1$x.2,pdr_2$x.2)
  
  pdr_new = ggplot(pdr_12, aes(pdr_12$pdr_1.Group.1.2)) +
    geom_line(aes(y=pdr_12$pdr_1.x.2,colour="no" )) +
    geom_line(aes(y=pdr_12$pdr_2.x.2,colour="yes") )  +
    xlab('total offered load (Mbps)') +
    ylab('Mean packet drop rate at sender') +
    labs(color="Carrier Sensing") + scale_colour_manual(values=c("red","blue"))+
    ylim(c(0, 1))+
    xlim(c(0, 90))
  ggsave(paste(res.folder_result, '/ALLpdr_', '.pdf', sep=''), width=16/div, height=9/div)
  print(pdr_new)
  
}
