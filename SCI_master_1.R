######## FUNCTIONS ########
#### Function: Split the conditions by the 96-well plate layout ####
split_condition <- function(df) #df = the data frame of a data file
{
  #split the conditoins from the data file
  loc_map <- matrix(unique(df$location), ncol = 12, byrow = TRUE) #96-well map
  condition_loc <- list() #list of conditions and their loc
  for(i in 1:6) #6 conditions maximum in a plate
  {
    condition_loc[[i]] <- c(loc_map[,(2*i-1):(2*i)]) #same condition for every 2 columns
  }
  data_split <- lapply(condition_loc, function(x) df[df$location %in% x,]) #split the data frame to list, each element is a df for each condition
  return(data_split)
}

#### Function: Read and Preprocess Data ####
prepare_data <- function(file_name) # file_name = the elements of the file name vector
{
  #read the file
  test <- read.csv(file_name, header = TRUE, sep = "\t", fileEncoding = "UTF-16LE") #read the data file to a data frame
  #treatment names
  xls_name <- tail(strsplit(file_name, "/")[[1]], 1) #get only the EXCEL file name, remove parent directories
  treatment_names <- strsplit(xls_name, "-")[[1]][2:4] ##parse the condition segement in the file names 
  #parse the condition segement in the file names
  treatment_date <- gsub(".xls", "", paste(strsplit(xls_name, "-")[[1]][5:7], collapse = "-")) #parse the date of the plate
  #split the data by condition
  test_conSplit <- split_condition(test)
  #add the date column for each df in the list
  test_conSplit <- lapply(test_conSplit, function(x)
  {
    x$date <- rep(treatment_date, nrow(x))
    return(x)
  })
  #rename each sub data frame
  names(test_conSplit) <- paste(sapply(treatment_names, function(x) rep(x,2)),
                                rep(c("U", "I"), 3),
                                sep = "_")
  #remove the empty conditions
  empty_idx <- which(treatment_names %in% grep("empty",treatment_names,ignore.case=TRUE,value=TRUE)) #get the index of empty conditions in the file names, case-insensitive
  test_conSplit[c(2*empty_idx-1, 2*empty_idx)] <- NULL #remove the elements of emtpy conditions
  #rename the "aname" column
  for(i in 1:length(test_conSplit)) #loop through all elements in the list
  {
    test_conSplit[[i]]$aname <- rep(names(test_conSplit)[i], nrow(test_conSplit[[i]])) #rename the "aname" column with the treatment and injury conditions
    test_conSplit[[i]]$treatment <- strsplit(names(test_conSplit)[i], "_")[[1]][1] #remove the injury label from the "aname" column, create a "treatment column"
    test_conSplit[[i]]$injury <- strsplit(names(test_conSplit)[i], "_")[[1]][2] #add an "injury" column to to specify the injury condition
  }
  
  #parse the columns
  output_col <- c("date", "location", "aname", "treatment", "injury", "start", "end", "inadist", "smldist", "lardist")
  test_conSplit <- lapply(test_conSplit, function(x) x[,output_col])#subset the data frame by the selected columns
  #calculate the total distance
  test_conSplit <- lapply(test_conSplit,
                          function(x)
                          {
                            x$distance <- x$inadist + x$smldist + x$lardist
                            return (x)
                          }
  )
  
  return(test_conSplit)
}

#### Function: Read and Preprocess Data ####
cumulative_distance <- function(x, timeRange) #x = raw data for each condition; lightChange = the starting time for corresponding light change condition, 1800 sec = light on, 2400 sec = light off; timeRange = time range after light change, measured in seconds
{
  #calcuale cumulative distance in each specific range
  x <- x[x$start %in% timeRange, ] # first 10 mins
  x.agg <- aggregate(distance~date+location+aname+treatment+injury, x, sum) # total distance for every larva in the time frame, aggregate by location and aname column
  x.agg$start <- rep(min(timeRange),nrow(x.agg)) #start point
  x.agg$end <- rep(max(timeRange),nrow(x.agg)) #end point
  x.agg$zero <- rep(1, nrow(x.agg)) #add a column with zero flags
  return(x.agg)
}

MEAN_STDEV_STEDRR <- function(x.agg, remove_zero = FALSE, remove_outlier = FALSE)
{
  #remove 0s if flag is true
  if(remove_zero_flag)
  {
    x.agg <- x.agg[x.agg$zero !=0,]
  }
  
  x.agg.summaryStats <- c(mean(x.agg$distance), sd(x.agg$distance), std.error(x.agg$distance)) #calculate the group mean, standard deviation and standard error
  x.agg.summaryStats.df <- as.data.frame(matrix(x.agg.summaryStats, nrow = nrow(x.agg), ncol = length(x.agg.summaryStats), byrow = TRUE)) #convert the summary stats to data frame with the same number of rows as the total distance DF
  colnames(x.agg.summaryStats.df) <- c('MEAN', 'STDEV', 'STDERR') #rename the column names
  
  x.agg <- cbind(x.agg, x.agg.summaryStats.df) #add to the total distance DF
  
  x.agg$outlier <-  abs(x.agg$distance-x.agg$MEAN) > 2*x.agg$STDEV #cut-off for variability, MEAN +/- 2*STDEV; TRUE = outlier
  
  #remove outlier
  if(remove_outlier)
  {
    x.agg <- x.agg[x.agg$outlier == FALSE, ] #subset the set data by keeping only the non-outliers
    #recalculate the mean, sd and stderr
    x.agg.summaryStats <- c(mean(x.agg$distance), sd(x.agg$distance), std.error(x.agg$distance)) #calculate the group mean, standard deviation and standard error
    x.agg.summaryStats.df <- as.data.frame(matrix(x.agg.summaryStats, nrow = nrow(x.agg), ncol = length(x.agg.summaryStats), byrow = TRUE)) #convert the summary stats to data frame with the same number of rows as the total distance DF
    colnames(x.agg.summaryStats.df) <- c('MEAN', 'STDEV', 'STDERR') #rename the column names
    x.agg[,10:12] <- x.agg.summaryStats.df #replace the old summary stats
    
  }
  
  x.agg$remove_zero <- rep(remove_zero, nrow(x.agg)) #add a column to flag if zeros are removed
  x.agg$remove_outlier <- rep(remove_outlier, nrow(x.agg))
  return(x.agg)
}

#### Function: Hypothesis Testing and Write to Files ####
stat_tests <- function(x, remove_outlier = FALSE, out)
{
  file_ext <- "" #place holder for no clean
  
  if(remove_outlier) #if remove_outlier == TRUE, subset the data by outlier
  {
    x <- x[x$outlier == FALSE,]
    file_ext <- "_CLEAN"
  }
  
  x_list <- split(x, f = as.factor(x$aname))
  shapiro_list <- lapply(x_list, function(y) as.data.frame(shapiro_test(y$distance)[,2:3])) #shapiro-wilk test on normality for each group of data
  shapiro_df <- do.call('rbind', shapiro_list) #bind all dfs by rows
  shapiro_df$normality <- shapiro_df$p.value > 0.05 #call for normality by passing p-value cut-off of 0.05
  output_file <- paste(out, unique(x$date), "_shapiro_wilk_normality_", unique(x$start), "_", unique(x$end), file_ext, ".csv", sep = "") #specify the output file name
  write.csv(shapiro_df, output_file, quote = FALSE, row.names = TRUE) #write the resutl to a file #keep the row.names arugment TRUE to specify the name of the group name
  
  x.ttest <- t_test(x, formula = distance~aname, p.adjust.method = "fdr") #t-test, p value adjusted by FDR
  
  output_file <- paste(out, unique(x$date), "_stats_tTest_cumulative_distance_", unique(x$start), "_", unique(x$end), file_ext, ".csv", sep = "") #specify the output file name
  write.csv(x.ttest, output_file, quote = FALSE, row.names = FALSE) #save the t-test result to a csv file
  
  x.anova <- aov(x, formula = distance~aname) #ANOVA
  output_file <- paste(out, unique(x$date), "_stats_ANOVA_cumulative_distance_", unique(x$start), "_", unique(x$end), file_ext, ".txt", sep = "") #specify the output file name
  writeLines(capture.output(summary(x.anova)), output_file) #capture the summary result
  
  x.Tukey <- tukey_hsd(x.anova) #post-hoc Tukey HSD
  output_file <- paste(out, unique(x$date), "_stats_TukeyHSD_cumulative_distance", unique(x$start), "_", unique(x$end), file_ext, ".csv", sep = "") #specify the output file name
  write.csv(x.Tukey, output_file, quote = FALSE, row.names = FALSE) #save the TukeyHSD to a csv file
  
  return(list(x.ttest, x.anova, x.Tukey))
}


#### Function: Plot Bar Chart and  ####
bar_box_plots <- function(x, remove_outlier = FALSE, out) #a function to wrap ggplot functions; x = input data frame, as one of the cumulative distance data frame
{
  file_ext <- "" #place holder for no clean
  
  if(remove_outlier) #if remove_outlier == TRUE, subset the data by outlier
  {
    x <- x[x$outlier == FALSE,]
    file_ext <- "_CLEAN"
  }
  
  x$aname <- factor(x$aname, levels = c(neg_ctrl, setdiff(names(first_list), neg_ctrl)))#reorder the labels
  x$treatment <- factor(x$treatment, levels = c("DMSO", setdiff(unique(x$treatment), "DMSO")))#reorder the labels
  x$injury <- factor(x$injury, levels = c("U", "I"))
  
  ####### bar #######
  bar_input <- x[,c('date', 'aname', 'treatment','injury','MEAN', 'STDEV', 'STDERR', 'start', 'end')] #subset relevent columns for bar chart
  bar_input <- aggregate(.~date+aname+treatment+injury+start+end, bar_input, unique) #get the group MEAN, STDEV and STDERR, use as input
  
  bar_title <- paste("Time Period: (", bar_input$start, " s - ", bar_input$end, " s)", sep = "") #specify the title for the barchart
  
  bar_y_range <- c(0, ifelse(unique(x$end) - unique(x$start) == 599, 150, 1)) # y min = 0; y max = 1 for 5 secs, but = 60 for 10 minutes
  bar_file <- paste(out, unique(x$date), "_cumulateive_distance_BarChart_", unique(x$start), "_", unique(x$end), file_ext, ".pdf", sep = "") #specify the file name
  
  
  #ggplot object for barchart
  bar_chart <- ggplot(bar_input, aes(x = treatment, y = MEAN, fill = injury), group = 1) + #x, y and fill color by injury groups
    geom_bar(stat = "identity", position = position_dodge()) + #plot as bar chart
    geom_errorbar(aes(ymin = MEAN-STDERR, ymax = MEAN+STDERR), width = 0.5, position = position_dodge(0.9)) + #add error bar; NOTE: chagne to STDEV here if needed, currently using STDERR
    scale_fill_brewer(palette = "Paired") + #use a "paired" color scheme from ColorBrewer
    coord_cartesian(ylim = bar_y_range, expand = FALSE, clip = "off") + #specify the range of y-axis
    labs(title = bar_title, #add plot title
         x = NULL, #remove the x-axis title
         y = "Average Cumulative Distance (cm)") + #add the y-axis title 
    theme(plot.title = element_text(hjust = 0.5), #center the title
          axis.text.x = element_text(color="#000000", size = 10), #font color and size for the x-axis text
          axis.text.y = element_text(color="#000000", size = 10), #font color and size for the y-axis text
          legend.position = "top", #move the legend to top of the chart
          legend.title=element_blank(), #remove the legend title
          legend.text = element_text(size = 10), #font szie for legend
          legend.spacing = unit(1,"cm"), #add space in plot legend
          axis.title.y = element_text(size = 15), #font size for the y-axis title
          plot.margin = unit(c(0.5, 1, 0.5, 1.5), "cm")) + #add plot margin on the page, avoid element fall off the page
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) #add dodge to the x-axis test, avoid overlap
  
  pdf(bar_file) #create and open the pdf file
  plot(bar_chart) #plot the bar chart in this opend pdf file
  dev.off() #close the file
  
  ####### box #######
  box_y_range <- c(0, ifelse(unique(x$end) - unique(x$start) == 599, 200, 2)) # y min = 0; y max = 1 for 5 secs, but = 60 for 10 minutes
  box_file <- paste(out, unique(x$date), "_cumulateive_distance_BoxPlot_", unique(x$start), "_", unique(x$end), file_ext, ".pdf", sep = "") #specify the file name
  
  #box plot
  box_chart <- ggplot(x, aes_string(x = 'treatment', y = 'distance', fill = 'injury')) +
    geom_boxplot(outlier.shape=NA) + #remove the outlier from the boxplot, to avoid overlap with dot plot
    geom_point(position = position_jitterdodge(jitter.width = 0.2), aes(shape = injury, group = injury), size = 1.5) + #add the dots in the dot plot, separated by dot shape
    scale_fill_brewer(palette = "Paired") + #use a "paired" color scheme from ColorBrewer
    coord_cartesian(ylim = box_y_range, expand = FALSE, clip = "off") + #specify the range of y-axis
    labs(title = bar_title, #add plot title
         x = NULL, #remove the x-axis title
         y = "Average Cumulative Distance (cm)") + #add the y-axis title 
    theme(plot.title = element_text(hjust = 0.5), #center the title
          axis.text.x = element_text(color="#000000", size = 10), #font color and size for the x-axis text
          axis.text.y = element_text(color="#000000", size = 10), #font color and size for the y-axis text
          legend.position = "top", #move the legend to top of the chart
          legend.title=element_blank(), #remove the legend title
          legend.text = element_text(size = 10), #font szie for legend
          legend.spacing = unit(1,"cm"), #add space in plot legend
          axis.title.y = element_text(size = 15), #font size for the y-axis title
          plot.margin = unit(c(0.5, 1, 0.5, 1.5), "cm")) + #add plot margin on the page, avoid element fall off the page
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) #add dodge to the x-axis test, avoid overlap
  
 
  pdf(box_file) #create and open the pdf file
  plot(box_chart) #plot in this opened pdf file
  dev.off() #close the file
  
  return(list(bar_chart, box_chart)) #return to a list of ggplot objects, in case needed later
}

###################################  ###################################

#### MAIN ####
######## Settings and Parameters ########
#load the libraries
library(rstatix) #hypothesis testing
library(plotrix) #standard error
library(ggplot2) #plotting
#library(ggpubr) #add p-values and sig level on plots

#time point for analysis
#source("./input_output_parameters.R")

out_dir_1 <- paste0(out_dir, "output_", format(Sys.time(), "%Y-%m-%d_%H-%M-%Z"), "/") #use system time as 
light_on_dir <- paste(out_dir_1, "/light-on/", sep = "") #output dir for light-on results
light_off_dir <- paste(out_dir_1, "/light-off/", sep = "") #output dir for light-off results
#check if output directory is ready, if not create the subdirectories for output
dir.create(light_on_dir, recursive = TRUE) 
dir.create(light_off_dir, recursive = TRUE) 


######## Read Data and Preprocess ########
#read the file names in the working directory
file_list <- list.files(input_dir, pattern = "*.xls", full.names = TRUE) #a vector of file names
first_list <- unlist(lapply(file_list, prepare_data), recursive = FALSE) #return to a list, where each element is a data frame of a treatment condition

#calculate cumulative distance, flag for zeros and calculate simple stats
full_list <- lapply(first_list, function(x)
{
  #light on
  y1 <- cumulative_distance(x, light_seg_on) #cumulative distance for light on
  zero_on_idx <- which(y1$distance == 0)#get the index for rows that have 0 distance
  
  #light off
  y2 <- cumulative_distance(x, light_seg_off) #cumulative distance for light off
  zero_off_idx <- which(y2$distance == 0)#get the index for rows that have 0 distance
  
  zero_idx <- intersect(zero_on_idx, zero_off_idx) #find sample that have 0 in both conditions
  #change the zero status to 0 
  if(length(zero_idx) != 0)
  {
    y1[zero_idx,]$zero <- 0 #light on
    y2[zero_idx,]$zero <- 0 #light off
  }
  
  #calcualte the MEAN, STDEV, STDERR and flag outliers
  y1 <- MEAN_STDEV_STEDRR(y1, remove_zero = remove_zero_flag, remove_outlier = on_clean)
  y2 <- MEAN_STDEV_STEDRR(y2, remove_zero = remove_zero_flag, remove_outlier = off_clean)
  
  return(rbind (y1,y2))
  
})

#conver the list to data frame
full_list.df <- do.call('rbind', full_list) #join all dfs in the list by rows
out_file <- paste(out_dir_1, unique(full_list.df$date), "_All_Conditions_cumulateive_distance.csv", sep = "") #specify the output file name
write.csv(full_list.df, out_file, quote = FALSE, row.names = FALSE) #save cumulative distance to a csv file

############## Light Cycle ##############
sum_data_1800 <- full_list.df[full_list.df$start == min(light_seg_on), ]
sum_data_1800.Test.list <- stat_tests(sum_data_1800, on_clean, light_on_dir) #iterrate through the dfs for hypothesis testing
sum_data_1800.plots <- bar_box_plots(sum_data_1800, on_clean, light_on_dir) #plotting

############## Dark Cycle ##############
sum_data_2400 <- full_list.df[full_list.df$start == min(light_seg_off), ]
sum_data_2400.Test.list <- stat_tests(sum_data_2400, off_clean, light_off_dir) #iterrate through the dfs for hypothesis testing
sum_data_2400.plots <- bar_box_plots(sum_data_2400, off_clean, light_off_dir) #plotting
