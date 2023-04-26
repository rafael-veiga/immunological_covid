library(tidyverse)
library(ComplexHeatmap)
library(circlize)
library(gridtext)
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

d2_d = read.csv("data_aux/heat_d2_d.csv")
d3_p = read.csv("data_aux/heat_d3_p.csv")
f = file("data_aux/heat_d2_d.csv","r")
lin_d2_d = readLines(f,1)
close(f)
lin_d2_d = unlist(strsplit(lin_d2_d, ","))
col_label_d2_d = lin_d2_d[5:14]


f = file("data_aux/heat_d3_p.csv","r")
lin_d3_p = readLines(f,1)
close(f)
lin_d3_p = unlist(strsplit(lin_d3_p, ","))
col_label_d3_p = lin_d3_p[5:14]

#d2_d
normal = d2_d[d2_d$gr_d2_d=="h",]
normal$Sample = NULL
normal$batch = NULL
normal$type = NULL
normal$gr_d2_d = NULL
c_nor = f_cor(normal)

detec = d2_d[d2_d$gr_d2_d=="d",]
detec$Sample = NULL
detec$batch = NULL
detec$type = NULL
detec$gr_d2_d = NULL
c_detec = f_cor(detec)
comp_detect = f_comp(c_nor,c_detec)

ndetec = d2_d[d2_d$gr_d2_d=="n",]
ndetec$Sample = NULL
ndetec$batch = NULL
ndetec$type = NULL
ndetec$gr_d2_d = NULL
c_ndetec = f_cor(ndetec)
comp_ndetect = f_comp(c_nor,c_ndetec)




col_fun = colorRamp2(c(1,0, -1), c("red","white", "blue"))
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
            column_title_side = "top",
        show_row_dend = FALSE,
        row_labels = gt_render(col_label_d2_d),
        row_names_gp = grid::gpar(fontsize = 10),
        cluster_rows = dend,
        row_names_side = "left",
        column_labels = gt_render(col_label_d2_d),
        column_names_rot = -30,
        column_names_gp = grid::gpar(fontsize = 9),
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
            column_title = "Dose 2 responders",
            column_title_side = "top",
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
            column_title = "Dose 2 non-responders",
            column_title_side = "top",
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
A =h+d+n 
draw(A,heatmap_legend_side = "right",column_title = "A",column_title_gp = gpar(fontsize = 16))
draw(lgd, x = unit(43, "cm"), y = unit(13, "cm"), just = c("right", "top"))
#######################################
#d3_d
normal = d3_p[d3_p$gr_d3_p=="h",]
normal$Sample = NULL
normal$batch = NULL
normal$type = NULL
normal$gr_d3_p = NULL
c_nor = f_cor(normal)

detec = d3_p[d3_p$gr_d3_p=="d",]
detec$Sample = NULL
detec$batch = NULL
detec$type = NULL
detec$gr_d3_p = NULL
c_detec = f_cor(detec)
comp_detect = f_comp(c_nor,c_detec)

ndetec = d3_p[d3_p$gr_d3_p=="n",]
ndetec$Sample = NULL
ndetec$batch = NULL
ndetec$type = NULL
ndetec$gr_d3_p = NULL
c_ndetec = f_cor(ndetec)
comp_ndetect = f_comp(c_nor,c_ndetec)

col_fun = colorRamp2(c(1,0, -1), c("red","white", "blue"))
colormap2 = colorRamp2(c(-2, 0, 2), c("darkgreen", "white", "tomato4"))

dend = as.dendrogram(hclust(dist(abs(c_nor))))
dend = color_branches(dend, k = 3)

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
            column_title_side = "top",
            show_row_dend = FALSE,
            row_labels = gt_render(col_label_d3_p),
            row_names_gp = grid::gpar(fontsize = 10),
            cluster_rows = dend,
            row_names_side = "left",
            column_labels = gt_render(col_label_d3_p),
            column_names_rot = -30,
            column_names_gp = grid::gpar(fontsize = 9),
            row_title = NULL,
            row_split = 3,
            column_split = 3,
            row_gap = unit(2, "mm"),
            column_gap = unit(2, "mm"),
            border = TRUE,
            cell_fun = cell_fun1,
            width = unit(8, "cm"), height = unit(8, "cm"),
            cluster_columns = dend)
h
##detect

d = Heatmap(f_diagonal(c_detec),name="Correlation",col = col_fun,
            column_title = "Dose 3 positive",
            column_title_side = "top",
            show_row_dend = FALSE,
            cluster_rows = dend,
            row_names_side = "left",
            show_column_names = FALSE,
            row_title = NULL,
            row_split = 3,
            column_split = 3,
            row_gap = unit(2, "mm"),
            column_gap = unit(2, "mm"),
            border = TRUE,
            cell_fun = cell_fun2,
            width = unit(8, "cm"), height = unit(8, "cm"),
            cluster_columns = dend)
d
##detect
n = Heatmap(f_diagonal(c_ndetec),name="Correlation",col = col_fun,
            column_title = "Dose 3 non-positive",
            column_title_side = "top",
            show_row_dend = FALSE,
            cluster_rows = dend,
            row_names_side = "left",
            show_column_names = FALSE,
            row_title = NULL,
            row_split = 3,
            column_split = 3,
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
draw(lgd, x = unit(43, "cm"), y = unit(12.6, "cm"), just = c("right", "top"))
