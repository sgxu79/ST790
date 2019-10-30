library(plotly)
setwd("C:/Users/15306/Desktop/ST790/predicting-a-pulsar-star")
pulsar = read.csv(file="pulsar_stars.csv")
colnames(pulsar) = c("mip","sdip","ekip","sip","msnr","sdsnr","eksnr","ssnr","label")
p = plot_ly(pulsar, x = ~mip, y = ~ekip, z = ~sip, opacity = 0.5, colors = traffic,
            marker = list(color = ~label, size = 3)) %>% layout(scene = list(xaxis = list(title="Mean"),
                                                                                     yaxis = list(title="Excess Kurtosis"),
                                                                                     zaxis = list(title="Skewness"))) %>% add_markers() 
