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



RankDiff <- function(files = c("addition_results_v3.csv", "anagram_results.csv", "locality_results.csv"),
                        diffs = c("diff2", "letters", "city_frequency")){
  
  
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
    file = paste0(str_split(files[i],".csv")[[1]][1],"_bins.csv")
    write.csv(data.c, file = file)
    
  }
  # ggplot(data.c, aes(difficulty_bin)) + geom_histogram() + facet_grid(template_id~engine)
  
}

RankDiff()

# ggplot(data = filter(data, engine == "gpt-3.5-turbo"), aes(difficulty_bin, fill = outcome)) + geom_histogram(position = "fill")
# ggplot(data = filter(data, engine == "gpt-3.5-turbo"), aes(diff2, fill = outcome)) + geom_histogram(position = "fill")



# ------------------------------------------------------------------------------------------------
# Promting Plots ---------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

plotPrompts <- function(files = c("addition_results_v3_bins.csv", "anagram_results_bins.csv", "locality_results_bins.csv"),
                        diffs = c("diff2c", "letters", "city_frequency"),
                        # cuts = c(15,20,15),
                        # tlog = c(F,F,T),
                        outcome2show = "INCORRECT"){
  
  listPlots <- list()
  
  for(i in 1:length(files)){
    print(paste("------------", i))
    
    data <- read.csv(files[i])
    data[data$outcome=="AVOIDANCE", "outcome"] = "AVOIDANT"
    print(paste("------------", i))
    data.summ <- data %>% group_by(engine, template_id, difficulty_bin, outcome) %>% summarise(n = n(), .groups = "drop") %>% 
      complete(engine, template_id, difficulty_bin, outcome, fill = list(n=0)) %>% 
      group_by(engine, template_id, difficulty_bin) %>% mutate(freqOutcome = n / sum(n))
    
    
    data.summ.avgEachPrompt <-  data.summ %>% group_by(engine, template_id, outcome) %>% summarise(avgFreq = mean(freqOutcome))
    maxSlice <- data.summ.avgEachPrompt %>% group_by(engine, outcome) %>% slice(which.max(avgFreq)) # Which is the template with MAX freqOutcome for AVOIDANT, CORRECT, INCORRECT
    minSlice <- data.summ.avgEachPrompt %>% group_by(engine, outcome) %>% slice(which.min(avgFreq)) # Which is the template with MIN freqOutcome for AVOIDANT, CORRECT, INCORRECT
    
    maxSlice.data <- merge(data.summ, maxSlice, by = c("engine", "template_id", "outcome"))  
    minSlice.data <- merge(data.summ, minSlice, by = c("engine", "template_id", "outcome"))  
    
    data.summ$engine <- factor(data.summ$engine, 
                               levels = c("GPT-3 Ada","GPT-3 Babbage","GPT-3 Curie","GPT-3 Davinci","text-davinci-001",
                                          "text-davinci-002","text-davinci-003","GPT-3.5-turbo", "GPT-4"))
    
    listPlots[[i]] <- ggplot(filter(data.summ, outcome == outcome2show), aes(difficulty_bin, freqOutcome)) + 
      geom_point(aes(group = template_id), alpha = 0.05, size = 0.7) + 
      geom_line(aes(group = template_id), alpha = 0.05) + 
      geom_smooth(colour = "#a8dadc", fill = "#a8dadc", size = 0.7, linetype = "dashed") +
      
      geom_point(data = filter(maxSlice.data, outcome == outcome2show), aes(difficulty_bin, freqOutcome, group = template_id), colour = "#e63946", size = 0.7) + 
      geom_line(data = filter(maxSlice.data, outcome == outcome2show), aes(difficulty_bin, freqOutcome, group = template_id), colour = "#e63946", size = 0.7) + 
      geom_point(data = filter(minSlice.data, outcome == outcome2show), aes(difficulty_bin, freqOutcome, group = template_id), colour = "#1d3557", size = 0.7) +
      geom_line(data = filter(minSlice.data, outcome == outcome2show), aes(difficulty_bin, freqOutcome, group = template_id), colour = "#1d3557", size = 0.7) + 
      
      scale_x_continuous(expand=c(0, 1)) +
      
      geom_dl(data = filter(maxSlice.data, outcome == outcome2show), aes(label = template_id), method = list(dl.trans(x = x - 0.25, y = y+0.17), "last.points", cex = 0.85), colour = "#e63946") +
      geom_dl(data = filter(minSlice.data, outcome == outcome2show), aes(label = template_id), method = list(dl.trans(x = x - 0.4, y = y+0.45), "last.points", cex = 0.85), colour = "#1d3557") +
      
      facet_wrap(~factor(engine,levels=c("GPT-3 Ada","GPT-3 Babbage","GPT-3 Curie","GPT-3 Davinci","text-davinci-001",
                                         "text-davinci-002","text-davinci-003","GPT-3.5-turbo", "GPT-4")), nrow = 1) +  
      
      ylim(0, 1.06) +
      xlab("Difficulty") + ylab("") + 
      labs(title = stringr::str_split(files[i],"_")[[1]][1],
           subtitle = outcome2show) +
      theme_minimal() +
      theme(strip.text = element_text(face = "bold")) + 
      theme(
        plot.title = element_text(color = "black", size = 10, face = "bold"),
        plot.subtitle = element_text(color = "black", size = 6))
  }
  
  return(listPlots)
}



openPDFEPS(paste0("PromptSensitivity_INCORRECT"),width=16,height=10)
do.call("grid.arrange", c(plotPrompts(outcome = "INCORRECT"), ncol=1))
dev.off()

openPDFEPS(paste0("PromptSensitivity_CORRECT"),width=16,height=10)
do.call("grid.arrange", c(plotPrompts(outcome = "CORRECT"), ncol=1))
dev.off()

openPDFEPS(paste0("PromptSensitivity_AVOIDANT"),width=16,height=10)
do.call("grid.arrange", c(plotPrompts(outcome = "AVOIDANT"), ncol=1))
dev.off()





# ------------------------------------------------------------------------------------------------
# Perfomance Plots -------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

plotPerformance <- function(files = c("addition_results_v3_bins.csv", "anagram_results_bins.csv", "locality_results_bins.csv")){
  
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
    if(i == 3){
      listPlots[[i]] <- listPlots[[i]]  + xlab("Difficulty") + ylab("") 
        
    }else{
      listPlots[[i]] <- listPlots[[i]]  + xlab("") + ylab("") 
    }
      
  }
  
  return(listPlots)
}


openPDFEPS(paste0("Perfomance_area"),width=14,height=8)
do.call("grid.arrange", c(plotPerformance(), ncol=1))
dev.off()



