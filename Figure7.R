warnings()

#Load libraries:
library(ggplot2) #ggplot; ggsave
library(patchwork) #plotlayout

#Note: Change the input and output file location and filename

#Figure7a
#Data:
fig7a_data <- read.csv2(file = '../Fig7a.csv', 
                 header = TRUE,
                 dec=",",
                 sep = ";")


#Line graph:
f7roc1 <- ggplot(fig7a_data, aes(x = Specificity, y = Sensitivity, color = Colour)) +geom_path(size=1.5)+theme_bw() +labs(x='Specificity',y='Sensitivity') + scale_x_reverse( lim=c(1,0))
f7roc2 <- f7roc1 + scale_size_manual( values = 2 ) + labs( tag = "a") +
  geom_abline(slope=1, intercept=1, colour='grey50', linetype = 'dotted', size=1)+ theme(legend.position = "none")+
  theme(aspect.ratio=1,
        plot.title = element_text(size = 20,  face = "bold", hjust = 0.5),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        plot.tag = element_text(size = size_tag,  face = "bold"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.8))+
  scale_color_manual(values=c("#E69F00","#56B4E9","#008080"))

#Figure7b
#Data:
fig7b_data <- read.csv2(file = '/Users/ravikuma/Desktop/Ncomm_rebutal/data/fdata/Fig7b.csv', 
                        header = TRUE,
                        dec=",",
                        sep = ";")


#Line graph:
f7pr1 <- ggplot(data = fig7b_data, aes(x = Recall, y = Precision, color = Labels))+geom_path(size=1.5)+scale_y_continuous(limits = c(0, 1)) +theme_bw()
f7pr2 <- f7pr1 +theme(legend.position = "none")+  labs( tag = "b") +
  geom_hline(yintercept=min(fig7b_data$Precision), colour='grey50', linetype = 'dotted', size=1)+
  theme(aspect.ratio=1,
        plot.title = element_text(size = 20,  face = "bold", hjust = 0.5),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        plot.tag = element_text(size = 12,  face = "bold"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.8))+
  scale_color_manual(values=c("#E69F00","#56B4E9","#008080"))


#Save figure:
fig7 <- f7roc2 + f7pr2 + plot_layout(nrow = 1, ncol = 2) 
ggsave(file='../Fig7.pdf',
       plot = fig7,
       dpi = 300,
       width = 11.69, 
       height = 8.24, 
       units = "in",
       device = "pdf")
 
