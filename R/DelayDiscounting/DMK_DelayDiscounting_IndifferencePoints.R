############ Basic Data Load/Setup ############
library(reporttools)
library(xtable)
library(car)
library(lme4)
library(lmerTest)
library(ggplot2)
library(MASS)
library(lsmeans)
library(reshape)
library(lsr)
#library(LDdiag)
library(stats)
library(mediation)
library(lavaan)
library(emmeans)
#library(rstudioapi)
library(lubridate)

#### set up ####

#set working directory to location of script--not needed when called 
#through Rmarkdown doc. Uncomment below if running locally/manually
# library(rstudioapi)
# this.dir = getActiveDocumentContext()$path
# setwd(dirname(this.dir))

source('functions.R')

#### Load  Data ####
DMK_Demo = read.csv("ChildData/DMK_ReducedDemoDat.csv", na = "#NULL!")
DMK_DD_dat = read.csv("ChildData/DMK_DD_wide.csv", na = "#NULL!")
DD_rewards_delays = read.csv("ChildData/DD_rewardschedule.csv", na = "#NULL!") 

#### Clean Full Dataset ####
qMissing = colSums(is.na(DMK_DD_dat[2:92]))
DMK_DD_dat$nMissing = rowSums(is.na(DMK_DD_dat[2:92]))
DMK_DD_dat$nMissing_to90days = rowSums(is.na(DMK_DD_dat[2:67]))

DMK_DD_dat.long = melt(DMK_DD_dat, id.vars = names(DMK_DD_dat)[c()])
DMK_DD_dat.long$DD_choice = DMK_DD_dat.long$value
DMK_DD_dat.long$Question = DMK_DD_dat.long$variable 
DMK_DD_dat.long = DMK_DD_dat.long[c(1:7, 10:11)]

DMK_DD_dat.long = merge(DMK_DD_dat.long, DD_rewards_delays, id = "Question")

#### Get Indiference Points
delays = unique(DMK_DD_dat.long$DelayTime)
ids = unique(DMK_DD_dat.long$ID)

DMK_ip_dat = data.frame(ParID = rep(as.double(NA), length(ids)*(length(delays)-1)),
                        Delay = rep(as.double(NA), length(ids)*(length(delays)-1)),
                        IndifferencePoint = rep(as.double(NA), length(ids)*(length(delays)-1)),
                        IndifferencePoint_ratio = rep(as.double(NA), length(ids)*(length(delays)-1)),
                        ChecksPass = rep(as.double(NA), length(ids)*(length(delays)-1)),
                        ChecksExclude = rep(as.character(NA), length(ids)*(length(delays)-1)))

DMK_ip_dat$ChecksExclude = factor(DMK_ip_dat$ChecksExclude, levels = c("Y", "N"))

DMK_ipratio_dat.wide = data.frame(ParID = rep(as.double(NA), length(ids)),
                        ChecksPass = rep(as.double(NA), length(ids)),
                        ChecksExclude = rep(as.character(NA), length(ids)),
                        Delay0 = rep(as.double(NA), length(ids)),
                        Delay7 = rep(as.double(NA), length(ids)),
                        Delay30 = rep(as.double(NA), length(ids)),
                        Delay90 = rep(as.double(NA), length(ids)))

DMK_ipratio_dat.wide$ChecksExclude = factor(DMK_ipratio_dat.wide$ChecksExclude, levels = c("Y", "N"))
DMK_ipratio_dat.wide$Delay0 = 1
      
                                          

dcount = -1

for(p in 1:length(ids)){
  pstart = p*3 - 2
  pend = p*3
  
  DMK_ip_dat[pstart:pend, 1] = ids[p]
  DMK_ipratio_dat.wide[p, 1] = ids[p]
  
  for(d in 1:length(delays)){
    dat = DMK_DD_dat.long[DMK_DD_dat.long$ID == ids[p] & DMK_DD_dat.long$DelayTime == delays[d], ]
    
    if(delays[d] == 0){
      
      check1 = 1 == dat[dat$Question == "DD67", ]$DD_choice
      check2 = 0 == dat[dat$Question == "DD68", ]$DD_choice
      check3 = 0 == dat[dat$Question == "DD69", ]$DD_choice
      checks = sum(c(check1, check2, check3))
      DMK_ip_dat[pstart:pend, 5] = checks
      DMK_ip_dat[pstart:pend, 6] = as.character(ifelse(checks <=1, "Y", "N"))
      
      DMK_ipratio_dat.wide[p, 2] = checks
      DMK_ipratio_dat.wide[p, 3] = as.character(ifelse(checks <=1, "Y", "N"))
      
    } else{
      
      dcount = dcount + 1
      drow = dcount + pstart
      dcol = d + 4
      
      dat_a = dat[order(dat$NowValue), ]
      
      ascending = dat_a[dat_a$DD_choice == 1, 10]
      
      if(length(ascending) == 0){
        ip_ascending = NA
      } else if(length(ascending) == 22){
        ip_ascending = 0
      } else {
        ip_ascending = ascending[1]
      }
      
      dat_d = dat[order(-dat$NowValue), ]
      descending = dat_d[dat_d$DD_choice == 0, 10]
      
      if(length(descending) == 0){
        ip_descending = NA
      } else if(length(descending) == 10){
        ip_descending = 10
      } else {
        ip_descending = descending[1]
      }
      
      DMK_ip_dat[drow, 2] = delays[d]
      DMK_ip_dat[drow, 3] = median(c(ip_ascending[1], ip_descending[1]), na.rm = TRUE)
      DMK_ip_dat[drow, 4] = median(c(ip_ascending[1], ip_descending[1]), na.rm = TRUE)/10.5
      
      DMK_ipratio_dat.wide[p, dcol] = median(c(ip_ascending[1], ip_descending[1]), na.rm = TRUE)/10.5
    }
  }
  
  dcount = -1
}

write.csv(DMK_ip_dat, 'Data/DMK_DD_indiferencepoints.csv', row.names = FALSE)
write.csv(DMK_ipratio_dat.wide, 'Data/DMK_DD_indiferencepoints_wide.csv', row.names = FALSE)

