#plot for nup58
library(ggplot2)
nup49_data <- read.csv('/home/lavrentydanilov/Documents/Documents/Scientific_work/Genetics_and_biotechnology/LabDocuments/Nucleopeorine_functions/Scripts/Nucleoporines_id/NUP49/Sum_table_NUP49.csv', sep = ',', header = T)

nup49_plot <- ggplot(nup49_data, aes(X, NArches, color = Protein))+
  geom_line(size = 1.5)+facet_wrap(~Protein, ncol = 1) +
  theme_bw() +
  labs(x = "Аминокислотная позиция", y = "Суммарный индекс β-арок") +
  ggtitle("Гомологи NUP49")
nup49_data1 <- read.csv('/home/lavrentydanilov/Documents/Documents/Scientific_work/Genetics_and_biotechnology/LabDocuments/Nucleopeorine_functions/Scripts/Nucleoporines_id/NUP49/NUP49-FG.csv', sep = ',', header = T)



hum_49 <- nup49_data[nup49_data$Protein == 'Q9BVL2 (Homo sapiens)',]
hum_491 <- nup49_data1[nup49_data1$Protein == 'Q9BVL2 (Homo sapiens)',]


hum_491_sub <- hum_491[hum_491$FG != 0,]

nup49_plot <- ggplot(hum_49, aes(X, NArches))+
  geom_line(size = 1.5, color = 'blue') +
  theme_bw() +
  labs(x = "Аминокислотная позиция", y = "Суммарный индекс β-арок") +
  ggtitle("Белок NUP58")

nup49_plot +   geom_vline(xintercept = hum_491_sub$X, color = 'red')

  

sum_nup49_plot <- nup49_plot + 
  geom_bar(data = nup49_data1, aes(X, FG, color = Protein), stat = "identity", color = "NA", fill = "blue", alpha = 0.5) + 
  facet_wrap(~Protein, ncol = 1)

#plot for archcandy
setwd('/home/lavrentydanilov/Documents/Documents/Scientific_work/Genetics_and_biotechnology/Magistracy/Магистерская диссертация/images/SVG_for_pictures/Bioinf_part/')
library(dplyr)
library(ggplot2)
cummulat_data <- read.csv('result_table.tsv', sep = '\t', header = T)
cum_data_zero <- read.csv('result_table_add.tsv', sep = '\t', header = T, na.strings = 0)
cum_data_zero[cum_data_zero == 0] <- 'NA'

na_count <-sapply(cum_data_zero, function(y) (25 - sum(length(which(is.na(y)))))/25)
na_count <- data.frame(na_count)
na_count$ad <- na_count$na_count
na_count <-  na_count[c(2:nrow(na_count)),]
na_count$pos <- c(1:nrow(na_count))
na_count$variable <- 'cum_score'
row.names(na_count) <- c(1:nrow(na_count))

ggplot(na_count, aes(pos, variable, fill = as.numeric(na_count))) + geom_tile() + theme_bw()
heatmap(na_count_t)

arch_data <- na_count[c(1,3,4)]
arch_data <- arch_data[c(1:736),]

#plot for conservat
library(hrbrthemes)
library(viridis)
setwd('/home/lavrentydanilov/Documents/Documents/Scientific_work/Genetics_and_biotechnology/Magistracy/Магистерская диссертация/images/SVG_for_pictures/Bioinf_part/')
data_acid_acid <- read.csv('acid_to_acid.tsv', sep = '\t', header = T)
data_acid_acid$variable <- 'acid_to_acid'
data_goup <- read.csv('acid_to_group.tsv', sep = '\t', header = T)
data_goup$variable <- "group_group"
ggplot(data_acid_acid, aes(position, variable, fill = as.numeric(value))) + geom_tile() + theme_bw()
ggplot(data_goup, aes(position, variable, fill = as.numeric(value))) + geom_tile() + theme_bw()

colnames(arch_data) <- c('value', 'position', 'variable')



result <- bind_rows(data_acid_acid, data_goup)
result_1 <- bind_rows(result, arch_data)
color_palette <- colorRampPalette()[2207]
ggplot(result_1, aes(position, variable, fill = as.numeric(value))) + geom_tile() +
  scale_fill_viridis(discrete=FALSE) +
  theme_ipsum()
ggplot(result_1, aes(position, variable, fill = as.numeric(value))) + geom_tile() + 
scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red', midpoint = 0.5)
write.csv(result_1, 'data_for_plot.csv')
# 
data_acid_acid_1 <- as.data.frame(t(data_acid_acid[,2]))
colnames(data_acid_acid_1) <- c(1:736)
data_goup_1 <- as.data.frame(t(data_goup[,2]))
colnames(data_goup_1) <- c(1:736)

result_2 <- bind_rows(data_acid_acid_1, data_goup_1)
het <- as.matrix(result_2)
heatmap(het)

ggplot