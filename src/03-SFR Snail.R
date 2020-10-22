source("./lib/funcs.R")
library(rio)

#for these biplots, you only need the PCA for the latest year
#the all.pca list is arranged into dimensions (and an aggregate), by year
output_folders <- c("./data_out2020/")
#this is where you create THE list
aggr.pca.full <- data.frame(iso3c = all.pca$Aggregate$`2019`$iso3c, 
                            Fragility = all.pca$Aggregate$`2019`$x[,1])
aggr.pca.full <- aggr.pca.full %>% arrange((Fragility)) #arranges in descending order
aggr.pca.full$fragility.level <- "Rest of the World"
pos <- aggr.pca.full$Fragility <= -1.2 #this is the threshold for SFR 2018
aggr.pca.full$fragility.level[pos] <- "Fragile"
pos <- aggr.pca.full$Fragility <= -2.5 #this is the threshold for SFR 2018
aggr.pca.full$iso3c <- as.character(aggr.pca.full$iso3c)
aggr.pca.full$fragility.level[pos] <- "Extremely Fragile"
aggr.pca.full$country <- countrycode(aggr.pca.full$iso3c,
                                     "iso3c","un.name.en",custom_match = c("PSE" = "West Bank and Gaza Strip",
                                                                           "SWZ" = "Eswatini",
                                                                           "ROM" = "Romania"))
aggr.pca  <- aggr.pca.full %>% dplyr::select(iso3c,country,fragility.level,Fragility) %>% 
  dplyr::filter(fragility.level != "Rest of the World")

aggr.pca_adj <- aggr.pca %>% dplyr::select(iso3c,fragility.level,Fragility,location = country)
# Number of clusters - set to six
clusters = lapply(all.pca, function(x){
  temp = last(x)
  temp = cluster(temp, num.clusters = 6)
  temp = temp$labels %>% dplyr::select(iso3c, cluster)
})
clusters = bind_rows(clusters, .id = "dimension")

#note: using left_join instead of merge to preserve the orderings - this'll be important
#for creation of SFR snail
aggr.pca = left_join(aggr.pca,clusters,by = "iso3c") %>% dplyr::rename(fragility.score = Fragility)

#do this for the full list as well
aggr.pca.full <-  left_join(aggr.pca.full,clusters, by = "iso3c") %>% dplyr::rename(fragility.score = Fragility)

######import qualitative decisions on clusters
fragile.levels <- import("./data/additional data/dimensional fragility.xlsx")
aggr.pca <- left_join(aggr.pca,fragile.levels,by = c("dimension","cluster"))

aggr.pca$country <- oecd.country.name(aggr.pca$iso3c, short = F)
aggr.pca$country <- factor(aggr.pca$country, 
                           levels = unique(aggr.pca$country), ordered = T)

###do this for the full dataset as well, not just fragile contexts
aggr.pca.full <- left_join(aggr.pca.full,fragile.levels,by = c("dimension","cluster")) %>% 
  dplyr::filter(dimension != last(names(aggr.pca.full)))
aggr.pca.full$country <- oecd.country.name(aggr.pca.full$iso3c,short = F)
aggr.pca.full$country <- factor(aggr.pca.full$country,
                                levels = unique(aggr.pca.full$country),ordered = T)

##export the list, after incorporating the clusters
final_list <- aggr.pca %>% dplyr::select(iso3c,country,dimension,fragility.level,fragility.score) %>% 
  dplyr::filter(dimension == "Aggregate") %>% dplyr::select(-dimension)
write.xlsx(final_list,paste0(output_folders,"List of Fragile Contexts (2020).xlsx"))

# Step 8 - Pie Chart ------------------------------------------------------

aggr.pca  <- aggr.pca  %>% dplyr::select(country, dimension, fragility.level, Fragility)
angles <- -90 - 270/length(unique(aggr.pca$country)) * seq_along(aggr.pca$country)
angles <- angles%%360
pos <- angles > 90
angles[pos] <- angles[pos] - 180
aggr.pca$dimension <- factor(aggr.pca$dimension, levels = rev(c("Political", "Societal", "Economic", "Environmental", 
                                                                "Security")), ordered = T)

#COLOURS OF COUNTRY NAMES
ybreaks <- levels(aggr.pca$dimension)
aggr.pca$col <- brewer.pal(9, "Blues")[9]
aggr.pca$col[aggr.pca$fragility.level == "Fragile"] <- brewer.pal(9, "Blues")[6]
col <- aggr.pca  %>% dplyr::select(country, col) %>% distinct()
p <- ggplot(aggr.pca , aes(x = country, y = dimension)) + geom_tile(aes(fill = Fragility), alpha = 0.6, colour = grey(0.6)) 
upper.lim <- ceiling(10 * nlevels(aggr.pca$country))
cols <- brewer.pal(6, "Reds")

#COLOURS OF DIMENSIONAL SNAIL
p <- p + scale_fill_gradientn(colours = colorRampPalette(brewer.pal(9, "Blues")[9:4])(5), 
                              limits = c(1, 5), 
                              breaks = c(1, 5), 
                              labels = c("Severe", "Minor"))

p <- p + scale_x_discrete(expand = c(0.163, 0))
p <- p + scale_y_discrete(expand = c(0.5, 0))

p <- p + theme(axis.text.x = element_text(angle = angles, vjust = 2, colour = col$col))
p <- oecd.plot(p)

p <- p + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.text.y = element_blank(), 
               axis.ticks.y = element_blank(), legend.direction = "horizontal")
p <- p + coord_polar(start = -0.8)
p <- p + xlab("") + ylab("")

p <- p + geom_text(data = data.frame(x = 0.5, y = ybreaks, label = ybreaks), aes(x = x, y = y, label = label), 
                   inherit.aes = F, size = 3)

p <- p + theme(legend.position = c(0.2, 0.8), legend.background = element_rect(fill = rgb(233/255, 237/255, 
                                                                                          247/255)))
fname = paste0("./graphs/", length(unique(aggr.pca$country)), "-Fragile-Situations.pdf")
ggsave(p, filename = fname, height = 10, width = 20)