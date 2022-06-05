

rm(list=ls())
library(ggplot2)
library(tidyverse)
library(MASS) 
library(scales) 
dane = read.csv(file = "C:/Users/Acer/Desktop/magisterka/II semestr/statistical/prace domowe/dane.csv")
colnames(dane)[1] <- "problem"


library(data.table)
df <- setDT(dane)
df[,czas := time/time[language == 'c'], by = .(problem)]

  
library(data.table)
df <- setDT(dane)
df[,rozmiar := size/size[language == 'c'], by = .(problem)]


as.data.frame(df)
df$language <- factor(df$language, 
                      levels = c("Java", "Python", "Julia",
                                 "c"))

df$problem <- factor(df$problem, 
                      levels = c("n-body", "mandelbrot", "spectral norm",
                                 "fannkuch-redux", "fasta", "k-nucleotide", 
                                 "binary-trees", "reverse-complement", "pidigits",
                                 "regex-redux"))



x11()

    ggplot(df, aes(x = problem, y = czas, shape = language, color = language)) +
    geom_point(size=5) +
    scale_shape_manual(values = c(15,18,16,45))+
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x))) +
    labs(y="excetution time (relative to C)")+
    scale_color_manual(values = c("gray", "gray", "yellow", "orange"))+
    theme_minimal()+
    geom_hline(yintercept = 1, color = "orange", size=1.5)+
    theme(legend.title = element_blank(),
            legend.spacing.y = unit(0, "mm"), 
          axis.line = element_line(colour = "black"),
            aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12),
            legend.background = element_blank(),
            legend.box.background = element_rect(colour = "black"),
          axis.title.x=element_blank(),
          legend.position = c(0.8, 0.8),
          axis.ticks = element_line(size = 1, color="black"),
          axis.ticks.length = unit(0.3, "cm"),
          axis.text.x = element_text(angle = 30,vjust = 0.5, hjust=0.5))
    x11()
    
    ggplot(df, aes(x = problem, y = rozmiar, shape = language, color = language)) +
      geom_point(size=5) +
      scale_shape_manual(values = c(15,18,16,45))+
      labs(y="excetution size (relative to C)")+
      scale_color_manual(values = c("gray", "gray", "yellow", "orange"))+
      theme_minimal()+
      geom_hline(yintercept = 1, color = "orange", size=1.5)+
      theme(legend.title = element_blank(),
            legend.spacing.y = unit(0, "mm"), 
            axis.line = element_line(colour = "black"),
            aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12),
            legend.background = element_blank(),
            legend.box.background = element_rect(colour = "black"),
            axis.title.x=element_blank(),
            axis.text.x = element_text(angle = 30,vjust = 0.5, hjust=0.5),
            axis.ticks = element_line(size = 1, color="black"),
            axis.ticks.length = unit(0.3, "cm"),
            legend.position = c(0.8, 0.8))+
      scale_y_continuous(breaks = c(0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6))

  
