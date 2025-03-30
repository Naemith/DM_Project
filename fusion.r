library(dplyr)
library(readr)

tab_vip <- read.csv("/home/nae/Téléchargements/projR/medicinal_plant_taxo_focus_env.csv",sep=";")

tab_vip <- tab_vip[!(is.na(tab_vip$genus)),]
tab_vip <- tab_vip[!duplicated(tab_vip$species), ]
tab_vip <- tab_vip %>% select("complete_names", contains("_count"))

##################################################
datasamp <- read.csv("/home/nae/Téléchargements/projR/JBN_300_14_12_24_quant.csv",sep=",")

datasamp <- dplyr::select(datasamp, -row.m.z, -X, -row.retention.time)

datasamp <- t(datasamp)
datasamp <- data.frame(datasamp)

colnames(datasamp) <- datasamp[1,]
datasamp <- datasamp[-1,]


datasamp <- datasamp[, colSums(datasamp != 0) > 10]

datasamp$complete_names <- row.names(datasamp)
datasamp$complete_names <- gsub("^X|\\.Peak\\.area$", "", datasamp$complete_names)

tab_vip2 <- merge(tab_vip, datasamp, by = "complete_names", all = TRUE)

tab_vip2 <- dplyr::select(tab_vip2, -complete_names)

tab_vip2 <- tab_vip2 %>% mutate(across(matches("*"), ~ ifelse(. != 0, 1, 0)))

tab_vip2 <- na.omit(tab_vip2)

tab_vip2 <- tab_vip2[, !(grepl("^[0-9]", names(tab_vip2)) & colSums(tab_vip2, na.rm = TRUE) > 10)]

write.csv(tab_vip2, file = "/home/nae/Téléchargements/projR/final_table.csv", row.names = FALSE)
