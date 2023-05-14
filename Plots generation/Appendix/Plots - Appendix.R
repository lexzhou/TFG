# ------------------------------------------------------------------------------------------------
# Packages ---------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

options( java.parameters = "-Xmx12g" )
.lib<- c("tidyverse", "directlabels", "gridExtra")

.inst <- .lib %in% installed.packages()
if (length(.lib[!.inst])>0) install.packages(.lib[!.inst], repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
lapply(.lib, require, character.only=TRUE)

options(scipen=999)


# ------------------------------------------------------------------------------------------------
# Functions --------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

openPDFEPS <- function(file, height= PDFheight, width= PDFwidth, PDFEPS = 1) {
  if (PDFEPS == 1) {
    pdf(paste(file, ".pdf", sep=""), width, height)
  } else if (PDFEPS == 2) {
    postscript(paste(file, ".eps", sep=""), width, height, horizontal=FALSE)
  }
}


rank_binning <- function(df, diff_func="Difficulty", bin_num=30) {  # df is a dataframe about the results of one LLMs in a domain. diff_func is the difficulty function to be in the x-axis
  df <- df[order(df[[diff_func]]),]
  lis <- c()
  for (i in 1:bin_num) {
    num_inst_per_bin <- nrow(df) / bin_num
    for (j in 1:ceiling(num_inst_per_bin)) {
      lis <- append(lis, i)
    }
  }
  
  df$difficulty_bin <- lis[1:nrow(df)]
  return(df)
}


normalise <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}



RankDiff_addition <- function(files = c(rep("addition_results_v3.csv", 7),7),
                        diffs = c("diff1", "diff2", "diff3", "diffc", "diff1c", "diff2c", "diff3c")){
  
  
  for(i in 1:length(files)){
    print(paste("------------", i))
    data <- read.csv(files[i]) 
    list_of_dataframes <- list()
    
    index =1
    for (lm in unique(data$engine)){
      print(paste0(" -- ",lm))
      data.f <- filter(data, engine == lm)
      data.f <- rank_binning(data.f, diffs[i])
      list_of_dataframes[[index]] <- data.f
      index <- index + 1
    }
    data.c <- dplyr::bind_rows(list_of_dataframes, .id = "column_label")
    file = paste0(str_split(files[i],".csv")[[1]][1],"_bins_", diffs[i] , ".csv")
    write.csv(data.c, file = file)
    
  }
  # ggplot(data.c, aes(difficulty_bin)) + geom_histogram() + facet_grid(template_id~engine)
  
}

RankDiff_anagram <- function(files = c("anagram_results.csv", "anagram_results.csv"),
                         diffs = c("scrabble", "prob_inv")){
  
  
  for(i in 1:length(files)){
    print(paste("------------", i))
    data <- read.csv(files[i]) 
    list_of_dataframes <- list()
    
    index =1
    for (lm in unique(data$engine)){
      print(paste0(" -- ",lm))
      data.f <- filter(data, engine == lm)
      data.f <- rank_binning(data.f, diffs[i])
      list_of_dataframes[[index]] <- data.f
      index <- index + 1
    }
    data.c <- dplyr::bind_rows(list_of_dataframes, .id = "column_label")
    file = paste0(str_split(files[i],".csv")[[1]][1],"_bins_", diffs[i] , ".csv")
    write.csv(data.c, file = file)
    
  }
  # ggplot(data.c, aes(difficulty_bin)) + geom_histogram() + facet_grid(template_id~engine)
  
}

RankDiff_locality <- function(files = c(rep("locality_results.csv",4)),
                         diffs = c("city_prob_inv", "target_city_prob_inv", "multiplication_city_prob_inv", "city_frequency")){ # city_frequency = 1/city_popularity
  
  for(i in 1:length(files)){
    print(paste("------------", i))
    data <- read.csv(files[i]) 
    list_of_dataframes <- list()
    
    index =1
    for (lm in unique(data$engine)){
      print(paste0(" -- ",lm))
      data.f <- filter(data, engine == lm)
      data.f <- rank_binning(data.f, diffs[i])
      list_of_dataframes[[index]] <- data.f
      index <- index + 1
    }
    data.c <- dplyr::bind_rows(list_of_dataframes, .id = "column_label")
    file = paste0(str_split(files[i],".csv")[[1]][1],"_bins_", diffs[i] , ".csv")
    write.csv(data.c, file = file)
    
  }
  # ggplot(data.c, aes(difficulty_bin)) + geom_histogram() + facet_grid(template_id~engine)
  
}

# RankDiff_addition()
# RankDiff_anagram()
# RankDiff_locality()
# ggplot(data = filter(data, engine == "gpt-3.5-turbo"), aes(difficulty_bin, fill = outcome)) + geom_histogram(position = "fill")
# ggplot(data = filter(data, engine == "gpt-3.5-turbo"), aes(diff2, fill = outcome)) + geom_histogram(position = "fill")

# ------------------------------------------------------------------------------------------------
# Perfomance Plots -------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

plotPerformance_addition <- function(files = c("addition_results_v3_bins_diff1.csv", "addition_results_v3_bins_diff2.csv", 
                                               "addition_results_v3_bins_diff3.csv", "addition_results_v3_bins_diffc.csv",
                                               "addition_results_v3_bins_diff1c.csv", "addition_results_v3_bins_diff2c.csv",
                                               "addition_results_v3_bins_diff3c.csv"),
                                     diffs = c(latex2exp::TeX(r"($f_{min}$)"), latex2exp::TeX(r"($f_{hrm}$)"),
                                               latex2exp::TeX(r"($f_{art}$)"), latex2exp::TeX(r"($f_{cry}$)"),
                                               latex2exp::TeX(r"($f_{min} \oplus f_{cry}$)"), latex2exp::TeX(r"($f_{hrm} \oplus f_{cry}$)"),
                                               latex2exp::TeX(r"($f_{art} \oplus f_{cry}$)"))
                                     ){
  
  listPlots <- list()
  
  for(i in 1:length(files)){
    print(paste("------------", i))
    
    data <- read.csv(files[i])
    data[data$outcome=="AVOIDANCE", "outcome"] = "AVOIDANT"
    # data.summ <- data %>% group_by(engine, template_id, difficulty_bin, outcome) %>% summarise(n = n(), .groups = "drop") %>% 
    #   complete(engine, template_id, difficulty_bin, outcome, fill = list(n=0)) %>% 
    #   group_by(engine, template_id, difficulty_bin) %>% mutate(freqOutcome = n / sum(n))
    data.summ <- data %>% group_by(engine, template_id, difficulty_bin, outcome) %>% summarise(n = n()) %>% 
      mutate(freqOutcome = n / sum(n))

    ggplot(data = filter(data.summ, engine == "gpt-3.5-turbo"), aes(difficulty_bin, freqOutcome, fill = outcome)) + geom_col()
    
    data.summ$engine <- factor(data.summ$engine, 
                               levels = c("GPT-3 Ada","GPT-3 Babbage","GPT-3 Curie","GPT-3 Davinci","text-davinci-001",
                                          "text-davinci-002","text-davinci-003","GPT-3.5-turbo", "GPT-4"))
    
    data.summ$outcome <- factor(data.summ$outcome, 
                               levels = c("AVOIDANT", "INCORRECT", "CORRECT"))
    
    
    
    listPlots[[i]] <- ggplot(data.summ, aes(difficulty_bin, freqOutcome, colour = outcome, fill = outcome)) + 
      # geom_col(width=1.5) +
      geom_col(position = "fill") +
      facet_wrap(~engine, nrow = 1) + 
      scale_fill_manual("", labels = c("AVOIDANT", "INCORRECT", "CORRECT"),
                          values = c("#a8dadc", "#e63946", "#1d3557")) + 
      
      scale_colour_manual("", labels = c("AVOIDANT", "INCORRECT", "CORRECT"),
                        values = c("#a8dadc", "#e63946", "#1d3557")) + 
      # scale_y_continuous(labels = scales::percent) +
    
       
      labs(title = stringr::str_split(files[i],"_")[[1]][1]) +
      
      theme_minimal() +
      theme(strip.text = element_text(face = "bold")) + 
      theme(
        plot.title = element_text(color = "black", size = 10, face = "bold"),
        plot.subtitle = element_text(color = "black", size = 6))  +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      # theme(legend.position = c(0.05,0.8))  +
      # theme(legend.text = element_text(size = 6))
      
    if(i==1){
      listPlots[[i]] <- listPlots[[i]]  + 
        theme(
          legend.position = c(0.005,1.1),
          legend.justification = c("left", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6),
          legend.text = element_text(size = 6),
          legend.key = element_rect(fill = "white")
        )
    }else{
      listPlots[[i]] <- listPlots[[i]]  + 
        theme(
          legend.position = "none")
          
    }

    listPlots[[i]] <- listPlots[[i]]  + xlab(diffs[i]) + ylab("") 

  }
  
  return(listPlots)
}

plotPerformance_anagram <- function(files = c("anagram_results_bins_scrabble.csv", "anagram_results_bins_prob_inv.csv"),
                                    diffs = c(latex2exp::TeX(r"($f_{scb}$)"), 
                                              latex2exp::TeX(r"($f_{swf}$)"))
                                    ){
  
  listPlots <- list()
  
  for(i in 1:length(files)){
    print(paste("------------", i))
    
    data <- read.csv(files[i])
    data[data$outcome=="AVOIDANCE", "outcome"] = "AVOIDANT"
    data.summ <- data %>% group_by(engine, template_id, difficulty_bin, outcome) %>% summarise(n = n()) %>% 
      mutate(freqOutcome = n / sum(n))
    
    ggplot(data = filter(data.summ, engine == "gpt-3.5-turbo"), aes(difficulty_bin, freqOutcome, fill = outcome)) + geom_col()
    
    data.summ$engine <- factor(data.summ$engine, 
                               levels = c("GPT-3 Ada","GPT-3 Babbage","GPT-3 Curie","GPT-3 Davinci","text-davinci-001",
                                          "text-davinci-002","text-davinci-003","GPT-3.5-turbo", "GPT-4"))
    
    data.summ$outcome <- factor(data.summ$outcome, 
                                levels = c("AVOIDANT", "INCORRECT", "CORRECT"))
    
    
    
    listPlots[[i]] <- ggplot(data.summ, aes(difficulty_bin, freqOutcome, colour = outcome, fill = outcome)) + 
      # geom_col(width=1.5) +
      geom_col(position = "fill") +
      facet_wrap(~engine, nrow = 1) + 
      scale_fill_manual("", labels = c("AVOIDANT", "INCORRECT", "CORRECT"),
                        values = c("#a8dadc", "#e63946", "#1d3557")) + 
      
      scale_colour_manual("", labels = c("AVOIDANT", "INCORRECT", "CORRECT"),
                          values = c("#a8dadc", "#e63946", "#1d3557")) + 
      # scale_y_continuous(labels = scales::percent) +
      
      
      labs(title = stringr::str_split(files[i],"_")[[1]][1]) +
      
      theme_minimal() +
      theme(strip.text = element_text(face = "bold")) + 
      theme(
        plot.title = element_text(color = "black", size = 10, face = "bold"),
        plot.subtitle = element_text(color = "black", size = 6))  +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    # theme(legend.position = c(0.05,0.8))  +
    # theme(legend.text = element_text(size = 6))
    
    if(i==1){
      listPlots[[i]] <- listPlots[[i]]  + 
        theme(
          legend.position = c(0.005,1.1),
          legend.justification = c("left", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6),
          legend.text = element_text(size = 6),
          legend.key = element_rect(fill = "white")
        )
    }else{
      listPlots[[i]] <- listPlots[[i]]  + 
        theme(
          legend.position = "none")
      
    }
    
    listPlots[[i]] <- listPlots[[i]]  + xlab(diffs[i]) + ylab("") 
    
  }
  
  return(listPlots)
}


plotPerformance_locality <- function(files = c("locality_results_bins_city_prob_inv.csv", 
                                               "locality_results_bins_target_city_prob_inv.csv",
                                               "locality_results_bins_multiplication_city_prob_inv.csv",
                                               "locality_results_bins_city_frequency.csv"),
                                     diffs = c(latex2exp::TeX(r"($f_{inp}$)"), 
                                               latex2exp::TeX(r"($f_{tar}$)"),
                                               latex2exp::TeX(r"($f_{mul}$)"), 
                                               latex2exp::TeX(r"($f_{pop}$)"))
                                     ){
  
  listPlots <- list()
  
  for(i in 1:length(files)){
    print(paste("------------", i))
    
    data <- read.csv(files[i])
    data[data$outcome=="AVOIDANCE", "outcome"] = "AVOIDANT"
    data.summ <- data %>% group_by(engine, template_id, difficulty_bin, outcome) %>% summarise(n = n()) %>% 
      mutate(freqOutcome = n / sum(n))
    
    ggplot(data = filter(data.summ, engine == "gpt-3.5-turbo"), aes(difficulty_bin, freqOutcome, fill = outcome)) + geom_col()
    
    data.summ$engine <- factor(data.summ$engine, 
                               levels = c("GPT-3 Ada","GPT-3 Babbage","GPT-3 Curie","GPT-3 Davinci","text-davinci-001",
                                          "text-davinci-002","text-davinci-003","GPT-3.5-turbo", "GPT-4"))
    
    data.summ$outcome <- factor(data.summ$outcome, 
                                levels = c("AVOIDANT", "INCORRECT", "CORRECT"))
    
    
    
    listPlots[[i]] <- ggplot(data.summ, aes(difficulty_bin, freqOutcome, colour = outcome, fill = outcome)) + 
      # geom_col(width=1.5) +
      geom_col(position = "fill") +
      facet_wrap(~engine, nrow = 1) + 
      scale_fill_manual("", labels = c("AVOIDANT", "INCORRECT", "CORRECT"),
                        values = c("#a8dadc", "#e63946", "#1d3557")) + 
      
      scale_colour_manual("", labels = c("AVOIDANT", "INCORRECT", "CORRECT"),
                          values = c("#a8dadc", "#e63946", "#1d3557")) + 
      # scale_y_continuous(labels = scales::percent) +
      
      
      labs(title = stringr::str_split(files[i],"_")[[1]][1]) +
      
      theme_minimal() +
      theme(strip.text = element_text(face = "bold")) + 
      theme(
        plot.title = element_text(color = "black", size = 10, face = "bold"),
        plot.subtitle = element_text(color = "black", size = 6))  +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    # theme(legend.position = c(0.05,0.8))  +
    # theme(legend.text = element_text(size = 6))
    
    if(i==1){
      listPlots[[i]] <- listPlots[[i]]  + 
        theme(
          legend.position = c(0.005,1.1),
          legend.justification = c("left", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6),
          legend.text = element_text(size = 6),
          legend.key = element_rect(fill = "white")
        )
    }else{
      listPlots[[i]] <- listPlots[[i]]  + 
        theme(
          legend.position = "none")
      
    }
    
    listPlots[[i]] <- listPlots[[i]]  + xlab(diffs[i]) + ylab("") 
    
  }
  
  return(listPlots)
}



openPDFEPS(paste0("addition_diffs_comparison"),width=14,height=18.66)
do.call("grid.arrange", c(plotPerformance_addition(), ncol=1))
dev.off()

openPDFEPS(paste0("anagram_diffs_comparison"),width=14,height=5.66)
do.call("grid.arrange", c(plotPerformance_anagram(), ncol=1))
dev.off()

openPDFEPS(paste0("locality_diff_comparison"),width=14,height=10.66)
do.call("grid.arrange", c(plotPerformance_locality(), ncol=1))
dev.off()





### Including Spearman correlation
plotPerformance_addition_table <- function(files = c("addition_results_v3_bins_diff1.csv", "addition_results_v3_bins_diff2.csv", 
                                                     "addition_results_v3_bins_diff3.csv", "addition_results_v3_bins_diffc.csv",
                                                     "addition_results_v3_bins_diff1c.csv", "addition_results_v3_bins_diff2c.csv",
                                                     "addition_results_v3_bins_diff3c.csv"),
                                           diffs = c(latex2exp::TeX(r"($f_{min}$)"), latex2exp::TeX(r"($f_{hrm}$)"),
                                                     latex2exp::TeX(r"($f_{art}$)"), latex2exp::TeX(r"($f_{cry}$)"),
                                                     latex2exp::TeX(r"($f_{min} \oplus f_{cry}$)"), latex2exp::TeX(r"($f_{hrm} \oplus f_{cry}$)"),
                                                     latex2exp::TeX(r"($f_{art} \oplus f_{cry}$)"))

){
  
  spearman_corr_list <- list()
  
  for(i in 1:length(files)){
    print(paste("------------", i))
    
    data <- read.csv(files[i])
    data[data$outcome=="AVOIDANCE", "outcome"] = "AVOIDANT"
    data.summ <- data %>% group_by(engine, template_id, difficulty_bin, outcome) %>% summarise(n = n()) %>% 
      mutate(freqOutcome = n / sum(n))
    
    data.summ$engine <- factor(data.summ$engine, 
                               levels = c("GPT-3 Ada","GPT-3 Babbage","GPT-3 Curie","GPT-3 Davinci","text-davinci-001",
                                          "text-davinci-002","text-davinci-003","GPT-3.5-turbo", "GPT-4"))
    
    data.summ$outcome <- factor(data.summ$outcome, 
                                levels = c("AVOIDANT", "INCORRECT", "CORRECT"))
    
    # Calculate Spearman correlation for each engine
    corr_data <- data.summ[data.summ$outcome == "CORRECT", ]
    spearman_corr <- corr_data %>% group_by(engine) %>% 
      summarise(spearman_corr = cor(difficulty_bin, freqOutcome, method = "spearman")) %>% 
      mutate(spearman_corr = round(spearman_corr, 2))
    
    # Add file identifier to the table
    spearman_corr$file <- stringr::str_split(files[i],"_")[[1]][1]
    
    spearman_corr_list[[i]] <- spearman_corr
  }
  
  # Combine all Spearman correlation tables
  spearman_corr_table <- do.call(rbind, spearman_corr_list)
  
  return(spearman_corr_table)
}



### Including Spearman correlation
plotPerformance_anagram_table <- function(files = c("anagram_results_bins_scrabble.csv", "anagram_results_bins_prob_inv.csv"),
                                          diffs = c(latex2exp::TeX(r"($f_{scb}$)"), 
                                                    latex2exp::TeX(r"($f_{swf}$)"))

){
  
  spearman_corr_list <- list()
  
  for(i in 1:length(files)){
    print(paste("------------", i))
    
    data <- read.csv(files[i])
    data[data$outcome=="AVOIDANCE", "outcome"] = "AVOIDANT"
    data.summ <- data %>% group_by(engine, template_id, difficulty_bin, outcome) %>% summarise(n = n()) %>% 
      mutate(freqOutcome = n / sum(n))
    
    data.summ$engine <- factor(data.summ$engine, 
                               levels = c("GPT-3 Ada","GPT-3 Babbage","GPT-3 Curie","GPT-3 Davinci","text-davinci-001",
                                          "text-davinci-002","text-davinci-003","GPT-3.5-turbo", "GPT-4"))
    
    data.summ$outcome <- factor(data.summ$outcome, 
                                levels = c("AVOIDANT", "INCORRECT", "CORRECT"))
    
    # Calculate Spearman correlation for each engine
    corr_data <- data.summ[data.summ$outcome == "CORRECT", ]
    spearman_corr <- corr_data %>% group_by(engine) %>% 
      summarise(spearman_corr = cor(difficulty_bin, freqOutcome, method = "spearman")) %>% 
      mutate(spearman_corr = round(spearman_corr, 2))
    
    # Add file identifier to the table
    spearman_corr$file <- stringr::str_split(files[i],"_")[[1]][1]
    
    spearman_corr_list[[i]] <- spearman_corr
  }
  
  # Combine all Spearman correlation tables
  spearman_corr_table <- do.call(rbind, spearman_corr_list)
  
  return(spearman_corr_table)
}



plotPerformance_locality_table <- function(files = c("locality_results_bins_city_prob_inv.csv", 
                                                     "locality_results_bins_target_city_prob_inv.csv",
                                                     "locality_results_bins_city_frequency.csv"),
                                           diffs = c(latex2exp::TeX(r"($f_{inp}$)"), 
                                                     latex2exp::TeX(r"($f_{tar}$)"),
                                                     latex2exp::TeX(r"($f_{pop}$)"))
){
  
  spearman_corr_list <- list()
  
  for(i in 1:length(files)){
    print(paste("------------", i))
    
    data <- read.csv(files[i])
    data[data$outcome=="AVOIDANCE", "outcome"] = "AVOIDANT"
    data.summ <- data %>% group_by(engine, template_id, difficulty_bin, outcome) %>% summarise(n = n()) %>% 
      mutate(freqOutcome = n / sum(n))
    
    data.summ$engine <- factor(data.summ$engine, 
                               levels = c("GPT-3 Ada","GPT-3 Babbage","GPT-3 Curie","GPT-3 Davinci","text-davinci-001",
                                          "text-davinci-002","text-davinci-003","GPT-3.5-turbo", "GPT-4"))
    
    data.summ$outcome <- factor(data.summ$outcome, 
                                levels = c("AVOIDANT", "INCORRECT", "CORRECT"))
    
    # Calculate Spearman correlation for each engine
    corr_data <- data.summ[data.summ$outcome == "CORRECT", ]
    spearman_corr <- corr_data %>% group_by(engine) %>% 
      summarise(spearman_corr = cor(difficulty_bin, freqOutcome, method = "spearman")) %>% 
      mutate(spearman_corr = round(spearman_corr, 2))
    
    # Add file identifier to the table
    spearman_corr$file <- stringr::str_split(files[i],"_")[[1]][1]
    
    spearman_corr_list[[i]] <- spearman_corr
  }
  
  # Combine all Spearman correlation tables
  spearman_corr_table <- do.call(rbind, spearman_corr_list)
  
  return(spearman_corr_table)
}


spearman_corr_table_add <- plotPerformance_addition_table()
print(spearman_corr_table_add)

spearman_corr_table_ang <- plotPerformance_anagram_table()
print(spearman_corr_table_ang)

spearman_corr_table_loc <- plotPerformance_locality_table()
print(spearman_corr_table_loc)