warnings()

#Load libraries:
library(ggplot2) #ggplot; ggsave
library(patchwork) #plotlayout

#Note: Change the input and output file location and filename

#Parameters:
size_legend <-  2.9
size_text <- 8
size_tag <- 12
point_size <- 1.5

#Figure8a
#Data:
fig8a_data <- read.csv2(file = '../Fig8a.csv', 
                        header = TRUE,
                        dec=",",
                        sep = ";")
fig8a_spline_data <- read.csv2(file = '../Fig8a_spline.csv', 
                                header = TRUE,
                                dec=",",
                                sep = ";")


#Plot
fig8a <- ggplot(data=fig8a_data, aes(x=Single.dose.inhibition,y=Multi.dose.pkd)) +
                geom_point(aes(
                  colour = factor(Colours)),
                  alpha = 0.6,
                  shape = 16,
                  size = point_size
                ) +
                labs(
                  x = expression("Single-dose inhibition (%)"), # at 1µM 
                  #y = expression("Multi-dose pK"["d"]*" [M]"),
                  y = expression("Measured multi-dose pK"["d"]), 
                  tag = "a"
                ) + 
                scale_colour_manual(values = c("blue", rgb(0.2, 0.2, 0.2), "#00DA1D","red"), labels=NULL) + 
                theme_bw() + 
                theme(aspect.ratio=1, legend.position = "none",
                      plot.title = element_text(size = size_text,  face = "bold", hjust = 0.5),
                      axis.text = element_text(size = size_text),
                      axis.title = element_text(size = size_text),
                      legend.title = element_text(size = size_legend),
                      legend.text = element_text(size = size_legend),
                      # plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                      plot.tag = element_text(size = size_tag, face = "bold")
                ) + 
                geom_line(data = subset(fig8a_spline_data,Multi.dose.pkd>4.9), aes(x=Expected.single.dose.inhibition,y=Multi.dose.pkd), color='#202020', lwd = 0.6) + # black
                geom_hline(yintercept = 6, linetype = 'dotted', colour = '#808080', size = 0.7) +  #longdash
                geom_vline(xintercept = 80, linetype = 'dotted', colour = '#808080', size = 0.7)


#Figure8b
#Data:
fig8b_left_data <- read.csv2(file = '../Fig8b_left.csv', 
                        header = TRUE,
                        dec=",",
                        sep = ";")
fig8b_right_data <- read.csv2(file = '../Fig8b_right.csv', 
                               header = TRUE,
                               dec=",",
                               sep = ";")

#Plot-left
fig8b_left <- ggplot(fig8b_left_data, aes(x=Predicted.pkd, y = Measured.pkd,color =Colours)) +
  geom_point(size = point_size/1.3)+ 
  theme_bw() + 
  labs(
    x = expression("Predicted pK"["d"]), 
    y = expression("Measured pK"["d"]),
    tag = "b"
  ) + 
  theme(legend.position = "none")+scale_colour_manual(values=c('blue','black', '#00DA1D')) +
  geom_abline(slope = 1, intercept = 0, colour = '#808080', linetype = 'dotted', size = 0.7)+
  theme(
    plot.title = element_text(size = size_text,  face = "bold", hjust = 0.5),
    axis.text = element_text(size = size_text),
    axis.title = element_text(size = size_text),
    legend.title = element_text(size = size_legend),
    legend.text = element_text(size = size_legend),
    plot.tag = element_text(size = size_tag, face = "bold")
    # plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  ) +
  xlim( c(5,8.5) ) + 
  ylim( c(5,8.5) ) + 
  theme(aspect.ratio = 1) 

#Plot-right  
fig8b_right <- ggplot(fig8b_right_data, aes(x=Single.dose.inhibition, y = Expected.inhibition, color =Colours))+
  geom_point(size = point_size/1.3) + 
  theme_bw() + 
  labs(
    x = expression("Single-dose inhibition (%)"),  # at 1µM 
    y = expression("Expected inhibition (%)") # at 1µM 
  ) + 
  theme(legend.position = "none")+ 
  geom_abline(slope=1, intercept=0, colour = '#808080', linetype = 'dotted', size = 0.7) + 
  theme(
    plot.title = element_text(size = size_text,  face = "bold", hjust = 0.5),
    axis.text = element_text(size = size_text),
    axis.title = element_text(size = size_text),
    legend.title = element_text(size = size_legend),
    legend.text = element_text(size = size_legend)
    # plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  ) + 
  theme(aspect.ratio = 1) + 
  scale_colour_manual(values=c('blue','black', '#00DA1D'))

#Figure8c
#Data:
fig8c_left_data <- read.csv2(file = '../Fig8c_left.csv', 
                             header = TRUE,
                             dec=",",
                             sep = ";")
fig8c_right_data <- read.csv2(file = '../Fig8c_right.csv', 
                              header = TRUE,
                              dec=",",
                              sep = ";")

#Plot-left
fig8c_left <- ggplot(fig8c_left_data, aes(x=Predicted.pkd, y = Measured.pkd,color =Colours)) + 
  geom_point(size = point_size/1.3) + 
  theme_bw() + 
  labs(
    x = expression("Predicted pK"["d"]*""), 
    y = expression("Measured pK"["d"]*""),
    tag = "c"
  ) + 
  theme(legend.position = "none") + 
  scale_colour_manual(values=c('blue','black', '#00DA1D')) + 
  geom_abline(slope=1, intercept=0, colour = '#808080', linetype = 'dotted', size = 0.7) + 
  theme(
    plot.title = element_text(size = size_text,  face = "bold", hjust = 0.5),
    axis.text = element_text(size = size_text),
    axis.title = element_text(size = size_text),
    legend.title = element_text(size = size_legend),
    legend.text = element_text(size = size_legend),
    plot.tag = element_text(size = size_tag,  face = "bold")
    #plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  ) + 
  xlim( c(5,7.2) ) + 
  ylim( c(5,8) ) + 
  theme(aspect.ratio = 1)

#Plot-right
fig8c_right <- ggplot(fig8c_right_data, aes(x=Single.dose.inhibition, y = Expected.inhibition, color =Colours)) + 
  geom_point(size = point_size/1.3) + 
  theme_bw() + 
  labs(
    x = expression("Single-dose inhibition (%)"),  # at 1µM
    y = expression("Expected inhibition (%)")      # at 1µM 
  ) + 
  theme(legend.position = "none") + 
  geom_abline(slope=1, intercept=0, colour = '#808080', linetype = 'dotted', size = 0.7) + 
  theme(
    plot.title = element_text(size = size_text,  face = "bold", hjust = 0.5),
    axis.text = element_text(size = size_text),
    axis.title = element_text(size = size_text),
    legend.title = element_text(size = size_legend),
    legend.text = element_text(size = size_legend)
    #plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  ) + 
  theme(aspect.ratio = 1) + 
  scale_colour_manual(values=c('blue','black', '#00DA1D'))

#Figure8d
#Data:
fig8d_left_data <- read.csv2(file = '../Fig8d_left.csv', 
                             header = TRUE,
                             dec=",",
                             sep = ";")
fig8d_right_data <- read.csv2(file = '../Fig8d_right.csv', 
                              header = TRUE,
                              dec=",",
                              sep = ";")

#Plot-left
fig8d_left <- ggplot(fig8d_left_data, aes(x=Predicted.pkd, y = Measured.pkd,color =Colours)) + 
  geom_point(size = point_size/1.3) + 
  theme_bw() + 
  labs(
    x = expression("Predicted pK"["d"]*""), 
    y = expression("Measured pK"["d"]*""),
    tag = "d"
  ) + 
  theme(legend.position = "none") + 
  scale_colour_manual(values=c('blue','black', 'red')) + 
  geom_abline(slope=1, intercept=0, colour = '#808080', linetype = 'dotted', size = 0.7) + 
  theme(
    plot.title = element_text(size = size_text,  face = "bold", hjust = 0.5),
    axis.text = element_text(size = size_text),
    axis.title = element_text(size = size_text),
    legend.title = element_text(size = size_legend),
    legend.text = element_text(size = size_legend),
    plot.tag = element_text(size = size_tag,  face = "bold")
    #plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  ) + 
  xlim( c(5,7.2) ) + 
  ylim( c(5,8) ) + 
  theme(aspect.ratio = 1)

#Plot-right
fig8d_right <- ggplot(fig8d_right_data, aes(x=Single.dose.inhibition, y = Expected.inhibition, color =Colours)) + 
  geom_point(size = point_size/1.3) + 
  theme_bw() + 
  labs(
    x = expression("Single-dose inhibition (%)"),  # at 1µM
    y = expression("Expected inhibition (%)")      # at 1µM 
  ) + 
  theme(legend.position = "none") + 
  geom_abline(slope=1, intercept=0, colour = '#808080', linetype = 'dotted', size = 0.7) + 
  theme(
    plot.title = element_text(size = size_text,  face = "bold", hjust = 0.5),
    axis.text = element_text(size = size_text),
    axis.title = element_text(size = size_text),
    legend.title = element_text(size = size_legend),
    legend.text = element_text(size = size_legend)
    #plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  ) + 
  theme(aspect.ratio = 1) + 
  scale_colour_manual(values=c('blue','black', 'red'))

#Figure8e:
fig8e_data <- read.csv2(file = '../Fig8e.csv', 
                             header = TRUE,
                             dec=",",
                             sep = ";")

#Plot
fig8e <- ggplot(fig8e_data,aes(y = AU.ROC , x =Single.dose.inhibition.cut.off, color = Labels,linetype = Labels)) +
  geom_line(size=1.0) +
  theme_bw()+
  labs(
    x='Single-dose inhibition [%]', 
    y='AU-ROC',
    tag = "e") +
  scale_color_manual(values=c("#FF9999","#E69F00","#56B4E9","black","#008080"))+
  scale_linetype_manual(values = c("solid","solid","solid","dashed","solid")) +
  theme(legend.position = "none") +
  theme(
    plot.title = element_text(size = size_text,  face = "bold", hjust = 0.5),
    axis.text = element_text(size = size_text),
    axis.title = element_text(size = size_text),
    legend.title = element_text(size = size_text),
    legend.text = element_text(size = size_legend),
    plot.tag = element_text(size = size_tag, face = "bold"))+
  theme(aspect.ratio = 1)+
  geom_vline(xintercept=80, linetype='longdash',colour='grey50')+
  geom_hline(yintercept=0.5, linetype='longdash',colour='grey50')


#Save figure
layout_design <- "
11134
11134
11156
22256
22278
22278
"
fig8 <- fig8a + fig8e + fig8b_left + fig8b_right + fig8c_left + fig8c_right + fig8d_left + fig8d_right+ plot_layout(design = layout_design)
ggsave(file='../Fig8.pdf',
       plot = fig8,
       dpi = 300,
       width = 11.69, 
       height = 8.24, 
       units = "in",
       device = "pdf")
