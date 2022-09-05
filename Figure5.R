library(tidyverse)
library(ComplexHeatmap)
library(circlize)
library(dendextend)

f_cor <- function(df){
  df_cor = cor(df,use="pairwise.complete.obs",method="spearman")
  # remove diagonal
  #for(i in 1:length(df_cor[,1])){
  #  df_cor[i,i] = NA
  #}
  return(df_cor)
  
}

f_comp <- function(nor,df){
  comp = nor
  for(i in 1:length(nor[1,])){
    for(j in 1:length(nor[1,])){
      if(i>j){
        comp[i,j] = df[i,j]-nor[i,j]
        comp[j,i] = comp[i,j]
      }
    }
  }
  return(comp)
}

f_diagonal <- function(df){
  for(i in 1:length(df[,1])){
    df[i,i] = NA
  }
  return(df)
}

#conn <- file("data_aux/heat_1.csv",open="r")
#linn <-readLines(conn)
#col_label_d2_d = strsplit(linn[1], ",")[[1]]

colormap2 = colorRamp2(c(-2, 0, 2), c("darkolivegreen", "white", "tomato4"))

d2_d = read.csv("data_aux/heat_1.csv")
d2_d = d2_d[,0:(length(d2_d[0,])-2)]

d3_d = read.csv("data_aux/heat_2.csv")
d3_d = d3_d[,0:(length(d3_d[0,])-2)]

d2_d_v = colnames(d2_d)[0:(length(colnames(d2_d))-1)]
d3_d_v = colnames(d3_d)[0:(length(colnames(d3_d))-1)]
#d_d2_d = 1-cor(d2_d[,colnames(d2_d)[0:(length(colnames(d2_d))-1)]],method="spearman")
d2_d2 = read.csv("data_aux/heat_1.csv")
d2_d2 = d2_d2[,d2_d_v]
d3_d2 = read.csv("data_aux/heat_2.csv")
d3_d2 = d3_d2[,d3_d_v]
rm(d2_d_v)
rm(d3_d_v)
d2_d2$tp = d2_d$tp
d3_d2$tp = d3_d$tp 
d2_d = d2_d2
d3_d = d3_d2
rm(d2_d2)
rm(d3_d2)

#d2_d
normal = d2_d[d2_d$tp=="healthy",]
normal$tp = NULL
c_nor = f_cor(normal)

detec = d2_d[d2_d$tp=="dose 2 detectable",]
detec$tp = NULL
c_detec = f_cor(detec)
comp_detect = f_comp(c_nor,c_detec)

ndetec = d2_d[d2_d$tp=="dose 2 non-responders",]
ndetec$tp = NULL
c_ndetec = f_cor(ndetec)
comp_ndetect = f_comp(c_nor,c_ndetec)

col_fun = colorRamp2(c(1,0, -1), c("blue","white", "red"))
colormap2 = colorRamp2(c(-2, 0, 2), c("darkgreen", "white", "tomato4"))

dend = as.dendrogram(hclust(dist(abs(c_nor))))
dend = color_branches(dend, k = 5)

cell_fun1 <- function(j, i, x, y, width, height, fill) {
  if(i==j){
    grid.rect(x = x, y = y, width = width, height = height, gp = gpar(col = "grey", fill = "white"))
  }
}

cell_fun2 <- function(j, i, x, y, width, height, fill) {
  if(i==j){
    grid.rect(x = x, y = y, width = width, height = height, gp = gpar(col = "grey", fill = "white"))
    
  }else if(which(order.dendrogram(dend)==i) > which(order.dendrogram(dend)==j) ) {
    grid.rect(x = x, y = y, width = width, height = height, gp = gpar(col = "grey", fill = "white"))
    grid.circle(x = x, y = y, r = abs(comp_detect[i, j])/2 * min(unit.c(width, height)), 
                gp = gpar(fill = colormap2(comp_detect[i, j]), col = NA))
  } 
}

cell_fun3 <- function(j, i, x, y, width, height, fill) {
  if(i==j){
    grid.rect(x = x, y = y, width = width, height = height, gp = gpar(col = "grey", fill = "white"))
    
  }else if(which(order.dendrogram(dend)==i) > which(order.dendrogram(dend)==j) ) {
    grid.rect(x = x, y = y, width = width, height = height, gp = gpar(col = "grey", fill = "white"))
    grid.circle(x = x, y = y, r = abs(comp_ndetect[i, j])/2 * min(unit.c(width, height)), 
                gp = gpar(fill = colormap2(comp_ndetect[i, j]), col = NA))
  } 
}
## heathy
h = Heatmap(f_diagonal(c_nor),name="Correlation",col = col_fun,
            column_title = "Heathy",
            column_title_side = "bottom",
        show_row_dend = FALSE,
        row_labels = col_label_d2_d,
        cluster_rows = dend,
        row_names_side = "left",
        show_column_names = FALSE,
        row_title = NULL,
        row_split = 5,
        column_split = 5,
        row_gap = unit(2, "mm"),
        column_gap = unit(2, "mm"),
        border = TRUE,
        cell_fun = cell_fun1,
        width = unit(8, "cm"), height = unit(8, "cm"),
        cluster_columns = dend)
h
##detect

d = Heatmap(f_diagonal(c_detec),name="Correlation",col = col_fun,
            column_title = "Dose 2 responders",
            column_title_side = "bottom",
            show_row_dend = FALSE,
            cluster_rows = dend,
            row_names_side = "left",
            show_column_names = FALSE,
            row_title = NULL,
            row_split = 5,
            column_split = 5,
            row_gap = unit(2, "mm"),
            column_gap = unit(2, "mm"),
            border = TRUE,
            cell_fun = cell_fun2,
            width = unit(8, "cm"), height = unit(8, "cm"),
            cluster_columns = dend)
d
##detect
n = Heatmap(f_diagonal(c_ndetec),name="Correlation",col = col_fun,
            column_title = "Dose 2 non-responders",
            column_title_side = "bottom",
            show_row_dend = FALSE,
            cluster_rows = dend,
            row_names_side = "left",
            show_column_names = FALSE,
            row_title = NULL,
            row_split = 5,
            column_split = 5,
            row_gap = unit(2, "mm"),
            column_gap = unit(2, "mm"),
            border = TRUE,
            cell_fun = cell_fun3,
            width = unit(8, "cm"), height = unit(8, "cm"),
            cluster_columns = dend)
n
lgd = Legend(col_fun = colormap2, title = "Healthy",at = c(2, 0, -2),labels = c("low", "equal", "high"))
A =h+d+n 
draw(A,heatmap_legend_side = "right",column_title = "A",column_title_gp = gpar(fontsize = 16))
draw(lgd, x = unit(43, "cm"), y = unit(10, "cm"), just = c("right", "top"))
#######################################
#d3_d
normal = d3_d[d3_d$tp=="healthy",]
normal$tp = NULL
c_nor = f_cor(normal)

detec = d3_d[d3_d$tp=="dose 3 detectable",]
detec$tp = NULL
c_detec = f_cor(detec)
comp_detect = f_comp(c_nor,c_detec)

ndetec = d3_d[d3_d$tp=="dose 3 non-responders",]
ndetec$tp = NULL
c_ndetec = f_cor(ndetec)
comp_ndetect = f_comp(c_nor,c_ndetec)

col_fun = colorRamp2(c(1,0, -1), c("blue","white", "red"))
colormap2 = colorRamp2(c(-2, 0, 2), c("darkgreen", "white", "tomato4"))

dend = as.dendrogram(hclust(dist(abs(c_nor))))
dend = color_branches(dend, k = 4)

cell_fun1 <- function(j, i, x, y, width, height, fill) {
  if(i==j){
    grid.rect(x = x, y = y, width = width, height = height, gp = gpar(col = "grey", fill = "white"))
  }
}

cell_fun2 <- function(j, i, x, y, width, height, fill) {
  if(i==j){
    grid.rect(x = x, y = y, width = width, height = height, gp = gpar(col = "grey", fill = "white"))
    
  }else if(which(order.dendrogram(dend)==i) > which(order.dendrogram(dend)==j) ) {
    grid.rect(x = x, y = y, width = width, height = height, gp = gpar(col = "grey", fill = "white"))
    grid.circle(x = x, y = y, r = abs(comp_detect[i, j])/2 * min(unit.c(width, height)), 
                gp = gpar(fill = colormap2(comp_detect[i, j]), col = NA))
  } 
}

cell_fun3 <- function(j, i, x, y, width, height, fill) {
  if(i==j){
    grid.rect(x = x, y = y, width = width, height = height, gp = gpar(col = "grey", fill = "white"))
    
  }else if(which(order.dendrogram(dend)==i) > which(order.dendrogram(dend)==j) ) {
    grid.rect(x = x, y = y, width = width, height = height, gp = gpar(col = "grey", fill = "white"))
    grid.circle(x = x, y = y, r = abs(comp_ndetect[i, j])/2 * min(unit.c(width, height)), 
                gp = gpar(fill = colormap2(comp_ndetect[i, j]), col = NA))
  } 
}
## heathy
h = Heatmap(f_diagonal(c_nor),name="Correlation",col = col_fun,
            column_title = "Heathy",
            column_title_side = "bottom",
            show_row_dend = FALSE,
            row_labels = col_label_d2_d,
            cluster_rows = dend,
            row_names_side = "left",
            show_column_names = FALSE,
            row_title = NULL,
            row_split = 4,
            column_split = 4,
            row_gap = unit(2, "mm"),
            column_gap = unit(2, "mm"),
            border = TRUE,
            cell_fun = cell_fun1,
            width = unit(8, "cm"), height = unit(8, "cm"),
            cluster_columns = dend)
h
##detect

d = Heatmap(f_diagonal(c_detec),name="Correlation",col = col_fun,
            column_title = "Dose 3 responders",
            column_title_side = "bottom",
            show_row_dend = FALSE,
            cluster_rows = dend,
            row_names_side = "left",
            show_column_names = FALSE,
            row_title = NULL,
            row_split = 4,
            column_split = 4,
            row_gap = unit(2, "mm"),
            column_gap = unit(2, "mm"),
            border = TRUE,
            cell_fun = cell_fun2,
            width = unit(8, "cm"), height = unit(8, "cm"),
            cluster_columns = dend)
d
##detect
n = Heatmap(f_diagonal(c_ndetec),name="Correlation",col = col_fun,
            column_title = "Dose 3 non-responders",
            column_title_side = "bottom",
            show_row_dend = FALSE,
            cluster_rows = dend,
            row_names_side = "left",
            show_column_names = FALSE,
            row_title = NULL,
            row_split = 4,
            column_split = 4,
            row_gap = unit(2, "mm"),
            column_gap = unit(2, "mm"),
            border = TRUE,
            cell_fun = cell_fun3,
            width = unit(8, "cm"), height = unit(8, "cm"),
            cluster_columns = dend)
n
lgd = Legend(col_fun = colormap2, title = "Healthy",at = c(2, 0, -2),labels = c("low", "equal", "high"))
B =h+d+n

draw(B,heatmap_legend_side = "right",column_title = "B",column_title_gp = gpar(fontsize = 16))
draw(lgd, x = unit(43, "cm"), y = unit(10, "cm"), just = c("right", "top"))
