
#Compute the correlation matrix
clim_soil_merged <- as.matrix.data.frame(clim_soil_merged)
cormat <- (cor(clim_soil_merged))
melted_cormat<- melt(cormat) #reshapes data data frame into long df format

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Heatmap
png(filename="Soiltemp_factors.png", width = 5, height = 5, units = "in", pointsize=12, res=1200)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "lightskyblue3", high = "indianred3", mid = "white", 
                       midpoint = 0.5, limit = c(0,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  labs(title = "Climate",
       x="", y="")+
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 10, hjust = 1), 
        legend.title=element_blank(), legend.key.width = unit(0.3,'cm'), legend.key.height = unit(1,'cm'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  coord_fixed()
dev.off()

                      