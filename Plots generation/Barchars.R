library(ggplot2)
library(dplyr) 
library(reshape2)

df <- read.csv("PropAvoidance.csv")

openPDFEPS <- function(file, height= PDFheight, width= PDFwidth, PDFEPS = 1) {
  if (PDFEPS == 1) {
    pdf(paste(file, ".pdf", sep=""), width, height)
  } else if (PDFEPS == 2) {
    postscript(paste(file, ".eps", sep=""), width, height, horizontal=FALSE)
  }
}

df$Type <- factor(df$Type, levels = c("Spontaneously Non-conforming", "Spontaneously Epistemic", "Epistemic by Moderation", "Ethical by Moderation"))

dfm <- melt(df, id.vars = c("Task", "Type"))

levels(dfm$variable) <- c("GPT-3 Ada", "GPT-3 Babbage", "GPT-3 Curie", "GPT-3 Davinci", "text-davinci-001", "text-davinci-002", "text-davinci-003", "GPT-3.5 turbo", "GPT-4")
  
p <- ggplot(dfm, aes (variable, value, fill = Type, label = value)) + 
  geom_col(position = "stack") + 
  geom_text(data = filter(dfm, value > 0), position = position_stack(vjust = .5), color = "#FFFFFF", angle = 90) +
  facet_grid(.~Task, scales = "free_x") + 
  scale_fill_manual("", labels = c("Spontaneously Non-conforming", "Spontaneously Epistemic", "Epistemic by Moderation", "Ethical by Moderation"),
                    values = c("#1d3557", "#44bba4", "#a8dadc", "#e63946")) +
  guides(fill = guide_legend(nrow = 1)) +
  xlab("") + ylab("%")+
  theme_minimal(base_size = 16) +
  theme(strip.text = element_text(face = "bold")) + 
  theme(
    legend.position="top",
    plot.title = element_text(color = "black", size = 10, face = "bold"),
    plot.subtitle = element_text(color = "black", size = 6),
    axis.text.x = element_text(angle = 45, hjust = 1)
    )

openPDFEPS(paste0("AvoidanceProp"),width=14,height=5.5)
p
dev.off()

       