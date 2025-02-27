#install the following packages if run at the first time on a computer
#To install the packages, remove the leading hash "#" in the following line
#install.packages(c('rstatix','plotrix','ggplot2'))
#If packages are installed already, put a leading hash "#" in the line above, so that you do not need to install packages everytime you run

setwd("C:/Purdue_related/research/SCI_R/git-test/") #set the directory where this code and the "SCI_master_1.R" script are located. 
input_dir <- "C:/Purdue_related/research/SCI_R/git-test/example_data/data_1/" #specify alternative directory for where the input files are
out_dir <- "C:/Purdue_related/research/SCI_R/git-test/example_output/" #specify alternative path directory for where the output will be generated 

#specify the control groups to help with the order in plotting
neg_ctrl <- c("DMSO_U", "DMSO_I")

#parameters
light_seg_on <- 1800:(1800+5-1) # time segment for light on
light_seg_off <- 2400:(2400+10*60-1) # time segment for light off
on_clean <- TRUE #TRUE = remove the outliers, in light on data
off_clean <- TRUE #TRUE = remove the outliers, in light on data
remove_zero_flag <- TRUE #TRUE = remove the samples have cumulative 0 distance in both light on and off

source("SCI_master_1.R")