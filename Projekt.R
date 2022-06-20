# list.of.packages <- c("Hmisc","Stats","RColorBrewer","dplyr","magrittr","gridExtra","car","dunn.test","ggpubr","FSA")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

library("Hmisc","Stats")
library("RColorBrewer")
library("dplyr")
library("gridExtra")
library("ggplot2")
library("car")
library("dunn.test")
library("ggpubr")
library("FSA")

correlation<-function(x){
  if(x > 0) cat('Wspolczynnik korelacji: korelacja dodatnia \n')
  if(x == 0) cat('Wspolczynnik korelacji: brak korelacji \n')
  if(x < 0) cat('Wspolczynnik korelacji: korelacja ujemna \n')
  if(-1 < x && x < -0.7) cat('Sila korelacji: bardzo silna korelacja ujemna \n')
  if(-0.7 < x && x < -0.5) cat('Sila korelacji: silna korelacja ujemna \n')
  if(-0.5 < x && x < -0.3) cat('Sila korelacji: korelacja ujemna o srednim natezeniu \n')
  if(-0.3 < x && x < -0.2) cat('Sila korelacji: slaba korelacja ujemna \n')
  if(-0.2 < x && x < 0.2) cat('Sila korelacji: brak korelacji \n')
  if(0.2 < x && x < 0.3) cat('Sila korelacji: slaba korelacja dodatnia \n')
  if(0.3 < x && x < 0.5) cat('Sila korelacji: korelacja dodatnia o srednim natezeniu \n')
  if(0.5 < x && x < 0.7) cat('Sila korelacji: silna korelacja dodatnia \n')
  if(0.7 < x && x < 1) cat('Sila korelacji: bardzo silna korelacja dodatnia \n')
}


pdf("outliers_pt1.pdf")
par(mfrow=c(1,1))

args <- commandArgs(trailingOnly = TRUE)

#setwd('C:/Users/lukma/Desktop/Projekt_R')
#args<-"dane.csv"
if(length(args)==0){
  print("No files supplied.")
}else{
  dane<-read.csv2(args[1],sep = ";")
}


sink('raport.txt')
cat("##############################################################################\n")
cat(paste("Made changes:\n"))
for(i in 1:ncol(dane)) {
    if(is.numeric(dane[,i])){
      inserted_mean <- mean(dane[ , i], na.rm = TRUE)
      if(sum(is.na(dane[ , i]))!=0){
        cat(paste("Column:",i,'number of inserts: ',sum(is.na(dane[ , i])),'value: ',inserted_mean,'\n'))
      }
      dane[ , i][is.na(dane[ , i])] <- inserted_mean
    }
}
cat("##############################################################################\n")

names <- colnames(dane)
column_one <- dane[,1]
loop.vector <- 2:length(dane[1,])
cols <- brewer.pal(3, "Set3")


temp_col <- c()
for(i in 2:ncol(dane)){
  if(is.numeric(dane[,i])){
    temp_col<-append(temp_col,i)
  }
}
numeric_col<-dane[,c(1,temp_col)]
#attach(numeric_col)


write.csv2(dane,file="final.csv") 

for(i in loop.vector){
  if(is.numeric(dane[,i])){
    boxplot(dane[,i] ~ column_one,
            data=dane,
            plot=TRUE,
            col=cols,
            ylab =colnames(dane)[i])
  }
}
dev.off()


#######################pkt2
pdf("groups_characteristics_pt2.pdf")
par(mar=c(1,1,1,1))
grp <- c(names[1])

my_theme <- ttheme_minimal(
  core=list(bg_params = list(fill = blues9[1:4], col=NA),
            fg_params=list(fontface=3)),
  colhead=list(fg_params=list(col="lightblue", fontface=4L)),
  rowhead=list(fg_params=list(col="lightpink", fontface=3L)))

for(iter in 1:ncol(dane)){
  if(is.numeric(dane[,iter])){
    i <- colnames(dane)[iter]
    podsumowanie <-group_by(dane,across(all_of(grp))) %>%
      summarise(
        count = n(),
        mean = format(round(mean(eval(parse(text = i)),na.rm=TRUE),2),nsmall=2),
        sd = format(round(sd(eval(parse(text = i)),na.rm=TRUE),2),nsmall=2),
        median = format(round(median(eval(parse(text = i)),na.rm=TRUE),2),nsmall=2),
        Shapiro.stat = shapiro.test(eval(parse(text = i)))$statistic,
        Shapiro.p_value = shapiro.test(eval(parse(text = i)))$p.value
      )
    plot.new()
    grid.table(podsumowanie,theme=my_theme)
    text(.9, .9,i,font=4,cex=2,col="blue")
  }
}
dev.off()
#######################pkt2




#######################pkt3
pdf("groups_comparison_pt3.pdf")


if(length(unique(dane[,1])) > 2){
  for(i in temp_col){
    if(is.numeric(dane[,i])){
      
      column_name <- colnames(dane)[i]
      column_values <- dane[,column_name]
      ShapiroTest <-group_by(dane,across(all_of(grp))) %>%
        summarise(
          statistic = shapiro.test(column_values)$statistic,
          p_value = shapiro.test(column_values)$p.value
        )
      LeveneTest <- leveneTest(eval(parse(text = column_name))~eval(parse(text = names[1])),data=dane)$"Pr(>F)"
      
      density<-ggdensity(dane, x = column_name,
                         color = names[1], fill = names[1],
                         palette = c("#99cc00","#660099","#0047b3"),
                         ylab = 'gestosc', xlab = column_name
                         ) + facet_wrap(~ eval(parse(text = names[1])), scales = 'free')
      print(density)
      
      
      if(all(ShapiroTest$p_value > 0.05) && LeveneTest > 0.05){ # parametryczne ANOVA
          cat("##############################################################################\n")
          cat("Parametr: ",column_name," levene p.value: ", LeveneTest[1]," dane normalne i homogeniczne TEST: ANOVA","\n")
        
        AOVTest<-summary(aov(column_values~eval(parse(text = names[1])),data=dane))[[1]][["Pr(>F)"]][[1]]
        if(AOVTest < 0.05){
          cat("AOV p.value: ",AOVTest," < 0.05 - sa roznice pomiedzy grupami \n")
          tukeyTest <- TukeyHSD(aov(column_values~column_one, data = dane))
          print(tukeyTest)
          plot(tukeyTest)
          mtext(column_name,side = 1, col = "blue", cex = 1)
          mtext("TEST: ANOVA",side = 4, col = "red", cex = 1)
        }
        else{
          cat("AOV p.value: ",AOVTest," > 0.05 - brak roznic pomiedzy grupami \n\n")
        }
        
      }
      else{ # nie parametryczne Kruskal-Wallis
        cat("##############################################################################\n")
        cat("Parametr: ",column_name," levene p.value: ", LeveneTest[1],"dane nienormalne TEST: KRUSKAL-WALLIS","\n")
        pvalueKWtest<-kruskal.test(column_values ~ eval(parse(text = names[1])),data=dane)$p.value
        
        if(pvalueKWtest<0.05){
          cat(pvalueKWtest,"< 0.05 - sa roznice pomiedzy grupami \n")
          dunTest <- dunnTest(column_values,column_one)
          print(dunTest)
        }
        if(pvalueKWtest>0.05)
          cat(pvalueKWtest,"> 0.05 - brak roznic pomiedzy grupami \n\n")
      }
    }
  }
}
if(length(unique(dane[,1])) == 2){
  for(i in temp_col){
    if(is.numeric(dane[,i])){
      column_name <- colnames(dane)[i]
      column_values <- dane[,column_name]
      ShapiroTest <- shapiro.test(column_values)$p.value
      LeveneTest <- leveneTest(eval(parse(text = column_name))~eval(parse(text = names[1])),data=dane)$"Pr(>F)"
      
      if(ShapiroTest > 0.05 && LeveneTest > 0.05){
        cat("##############################################################################\n")
        cat("Parametr: ",column_name," Shapiro p.value: ", ShapiroTest[1]," levene p.value: ", LeveneTest[1]," dane normalne i homogeniczne TEST: t-Studenta","\n")
        StudentTest <- t.test(eval(parse(text = column_name))~eval(parse(text = names[1])),data=dane,var.equal=TRUE)
        if(StudentTest$p.value < 0.05){
          cat("t-Student Test p.value: ",StudentTest$p.value," < 0.05 - sa roznice pomiedzy grupami \n")
        }
        if(StudentTest$p.value > 0.05)
          cat("t-Student Test p.value: ",StudentTest$p.value," > 0.05 - brak roznic pomiedzy grupami \n\n")
      }
      
      if(ShapiroTest > 0.05 && LeveneTest < 0.05){
        cat("##############################################################################\n")
        cat("Parametr: ",column_name," Shapiro p.value: ", ShapiroTest[1]," levene p.value: ", LeveneTest[1]," dane normalne i niehomogeniczne TEST: Welcha","\n")
        WelchTest <- t.test(eval(parse(text = column_name))~eval(parse(text = names[1])),data=dane,var.equal=FALSE)
        if(WelchTest$p.value < 0.05){
          cat("Welch Test p.value: ",WelchTest$p.value," < 0.05 - sa roznice pomiedzy grupami \n")
        }
        if(WelchTest$p.value > 0.05)
          cat("Welch Test p.value: ",WelchTest$p.value," > 0.05 - brak roznic pomiedzy grupami \n\n")
      }
      
      if(ShapiroTest < 0.05){
        cat("##############################################################################\n")
        cat("Parametr: ",column_name," Shapiro p.value: ", ShapiroTest[1]," levene p.value: ", LeveneTest[1]," dane nie normalne TEST: Wilcoxona","\n")
        WilcoxTest <- wilcox.test(eval(parse(text = column_name))~eval(parse(text = names[1])),data=dane)
        if(WilcoxTest$p.value < 0.05){
          cat("Wilcox Test p.value: ",WilcoxTest$p.value," < 0.05 - sa roznice pomiedzy grupami \n")
        }
        if(WilcoxTest$p.value > 0.05)
          cat("Wilcox Test p.value: ",WilcoxTest$p.value," > 0.05 - brak roznic pomiedzy grupami \n\n")
      }
      
    }
  }
}
if(length(unique(dane[,1])) < 2)
  cat("Plik zawiera jedna grupe \n")


dev.off()

#######################pkt3






#######################pkt4
pdf("Correlation_pt4.pdf")
par(mar=c(1,1,1,1))
for (i in unique(column_one)){
  groupa <- dane %>% filter(column_one==i)
  used<-c()
  
  for(iter1 in temp_col){
    x1<-colnames(groupa)[iter1]
    for (iter2 in temp_col){
      y1<-colnames(groupa)[iter2]
      
      if(!x1==y1){
        
        if(!iter1%in%used && !iter2%in%used){
          
          x2<-groupa[,iter1]
          y2<-groupa[,iter2]
          Shapiro.p_value_1 = shapiro.test(x2)$p.value
          Shapiro.p_value_2 = shapiro.test(y2)$p.value
          # cat('grupa:',i, iter1, iter2," parametr:",colnames(groupa)[iter1],colnames(groupa)[iter2],'\n')
          # print(Shapiro.p_value_1)
          # print(Shapiro.p_value_2)

          if(Shapiro.p_value_1 > 0.05 && Shapiro.p_value_2 > 0.05){ ## Pearson TEST
            wynikTestuKorelacjiPearsona<-cor.test(x2,y2,method="pearson")
            if(wynikTestuKorelacjiPearsona$p.value < 0.05){
              cat("##############################################################################\n")
              cat('grupa:',i,"parametr:",colnames(groupa)[iter1],colnames(groupa)[iter2],"metoda: Pearsona \n")
              cat("estamte: ",wynikTestuKorelacjiPearsona$estimate,'\n')
              cat("p.value: ",wynikTestuKorelacjiPearsona$p.value,'\n')
              correlation(wynikTestuKorelacjiPearsona$estimate)
              
              new<-ggscatter(groupa,x = x1, y = y1,
                        add = 'reg.line', conf.int = TRUE,
                        cor.coef = TRUE, cor.method = 'pearson',
                        col = 'lightblue',
                        color = colnames(groupa)[1], fill = colnames(groupa)[1],
                        xlab = x1, ylab = y1)  + annotate("text",  x=Inf, y = Inf, label = "metoda: Pearson", vjust=1, hjust=1)
              print(new)
            }
          }
          else{ ## Spearman TEST
            wynikTestuKorelacjiSpearmana<-cor.test(x2,y2,method="spearman")
            if(wynikTestuKorelacjiSpearmana$p.value < 0.05){
              cat("##############################################################################\n")
              cat('grupa:',i,"parametr:",colnames(groupa)[iter1],colnames(groupa)[iter2],"metoda: Spearman \n")
              cat("estamte: ",wynikTestuKorelacjiSpearmana$estimate,'\n')
              cat("p.value: ",wynikTestuKorelacjiSpearmana$p.value,'\n')
              correlation(wynikTestuKorelacjiSpearmana$estimate)
              
              new<-ggscatter(groupa,x = x1, y = y1,
                             add = 'reg.line', conf.int = TRUE,
                             cor.coef = TRUE, cor.method = 'spearman',
                             col = 'lightblue',
                             color = colnames(groupa)[1], fill = colnames(groupa)[1],
                             xlab = x1, ylab = y1)  + annotate("text",  x=Inf, y = Inf, label = "metoda: Spearman", vjust=1, hjust=1)
              print(new)
            }
          }
        }
      }
    }
    used<-append(used,iter1)
  }
}
dev.off()
#######################pkt4


sink()

