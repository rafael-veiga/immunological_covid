# Carregar pacotes necessários
library(readr)
library(cowplot)
library(tidyverse)
library(ComplexHeatmap)
library(circlize)
library(gridtext)
library(dendextend)
library(svglite)
library(ggpubr)

color_d2_d = "#B0BF1A"
color_d3_d = "#2ca02c"
color_d3_p = "#1f77b4"
color_d3_depos = "#C552FF"
color_negative = "#ff7f0e"
color_detectable = "#2ca02c"
color_positive = "#1f77b4"
color_health = "#7f7f7f"


fill_d2_d = c("Control" = color_health, "Undetectable" = color_negative, "Detectable" = color_d2_d)
fill_d3_p = c("Control" = color_health, "Non-Positive" = color_negative, "Positive" = color_d3_p)
fill_d3_detecble = c("Control" = color_health, "Undetectable" = color_negative, "Detectable" = color_d3_d)
fill_d3_depos = c("Control" = color_health, "Detectable" = color_detectable, "Positive" = color_positive)

col_fun = colorRamp2(c(1,0, -1), c("red","white", "blue"))
colormap2 = colorRamp2(c(-2, 0, 2), c("darkgreen", "white", "tomato4"))

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

#################################################################################
# violin
## d2_d

dados <- read_csv("./data_aux/fig_violin_d2_d.csv")
dados$group = as.factor(dados$group)
levels(dados$group) = c("Detectable","Control","Undetectable")
dados$group <- factor(dados$group, levels = c("Control", "Undetectable", "Detectable"))

plot1 <- ggplot(dados, aes(x = group, y = `CD80+ median  :in:  CD80+ CD11c- CD21+ Double Negative B Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "CD80+ DN1 B cells",
    x = "",
    y = "MFI CD80"
  ) +
  scale_fill_manual(
    values =  fill_d2_d
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot2 <- ggplot(dados, aes(x = group, y = `CD38+ median  :in:  CD38+ Non-Classical Monocytes`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") + # Adiciona bordas aos violinos
  geom_jitter(width = 0.2, size = 1, shape = 21, fill = "black", color = "black", alpha = 0.6) + # Ajusta os pontos
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + # Adiciona linhas de média
  labs(
    title = "CD38+ Non-Classical Monocytes",
    x = "",
    y = "MFI CD38"
  ) +
  #scale_y_continuous(limits = c(180, 275)) + # Ajusta os limites do eixo Y
  scale_fill_manual(
    values = fill_d2_d
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", # Remove a legenda
    plot.title = element_text(hjust = 0.5,size=8), # Centraliza o título
    axis.text.x = element_text(size = 6, face = "bold"), # Ajusta o texto do eixo X
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot3 <- ggplot(dados, aes(x = group, y = `CD57+ median  :in:  CD57+ Double Negative T Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") + # Adiciona bordas aos violinos
  geom_jitter(width = 0.2, size = 1, shape = 21, fill = "black", color = "black", alpha = 0.6) + # Ajusta os pontos
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + # Adiciona linhas de média
  labs(
    title = "CD57+ DN T Cells",
    x = "",
    y = "MFI CD57"
  ) +
  #scale_y_continuous(limits = c(180, 275)) + # Ajusta os limites do eixo Y
  scale_fill_manual(
    values = fill_d2_d
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", # Remove a legenda
    plot.title = element_text(hjust = 0.5,size=8), # Centraliza o título
    axis.text.x = element_text(size = 6, face = "bold"), # Ajusta o texto do eixo X
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )

plot4 <- ggplot(dados, aes(x = group, y = `CD38+ median  :in:  CD38+ CD27- CD4+ T Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") + # Adiciona bordas aos violinos
  geom_jitter(width = 0.2, size = 1, shape = 21, fill = "black", color = "black", alpha = 0.6) + # Ajusta os pontos
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + # Adiciona linhas de média
  labs(
    title = "CD38+ CD27- CD4+ T Cells",
    x = "",
    y = "MFI CD38"
  ) +
  #scale_y_continuous(limits = c(180, 275)) + # Ajusta os limites do eixo Y
  scale_fill_manual(
    values = fill_d2_d
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", # Remove a legenda
    plot.title = element_text(hjust = 0.5,size=8), # Centraliza o título
    axis.text.x = element_text(size = 6, face = "bold"), # Ajusta o texto do eixo X
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )

plot5 <- ggplot(dados, aes(x = group, y = `CD80+ median  :in:  CD80+ CD21- CD27+ B Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") + # Adiciona bordas aos violinos
  geom_jitter(width = 0.2, size = 1, shape = 21, fill = "black", color = "black", alpha = 0.6) + # Ajusta os pontos
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + # Adiciona linhas de média
  labs(
    title = "CD80+ CD21- CD27+ B Cells",
    x = "",
    y = "MFI CD80"
  ) +
  #scale_y_continuous(limits = c(180, 275)) + # Ajusta os limites do eixo Y
  scale_fill_manual(
    values = fill_d2_d
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", # Remove a legenda
    plot.title = element_text(hjust = 0.5,size=8), # Centraliza o título
    axis.text.x = element_text(size = 6, face = "bold"), # Ajusta o texto do eixo X
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )

plot6 <- ggplot(dados, aes(x = group, y = `BAFF-R+ median  :in:  BAFF-R+ CD11c- CD21+ Double Negative B Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") + # Adiciona bordas aos violinos
  geom_jitter(width = 0.2, size = 1, shape = 21, fill = "black", color = "black", alpha = 0.6) + # Ajusta os pontos
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + # Adiciona linhas de média
  labs(
    title = "BAFF-R+ DN1 B cells",
    x = "",
    y = "MFI BAFF-R"
  ) +
  #scale_y_continuous(limits = c(180, 275)) + # Ajusta os limites do eixo Y
  scale_fill_manual(
    values = fill_d2_d
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", # Remove a legenda
    plot.title = element_text(hjust = 0.5,size=8), # Centraliza o título
    axis.text.x = element_text(size = 6, face = "bold"), # Ajusta o texto do eixo X
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )

plot7 <- ggplot(dados, aes(x = group, y = `CD80+ median  :in:  CD80+ CD21- B Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") + # Adiciona bordas aos violinos
  geom_jitter(width = 0.2, size = 1, shape = 21, fill = "black", color = "black", alpha = 0.6) + # Ajusta os pontos
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + # Adiciona linhas de média
  labs(
    title = "CD80+ CD21- B Cells",
    x = "",
    y = "MFI CD80"
  ) +
  #scale_y_continuous(limits = c(180, 275)) + # Ajusta os limites do eixo Y
  scale_fill_manual(
    values = fill_d2_d
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", # Remove a legenda
    plot.title = element_text(hjust = 0.5,size=8), # Centraliza o título
    axis.text.x = element_text(size = 6, face = "bold"), # Ajusta o texto do eixo X
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )
# Continue criando os plots como acima para cada variável
plot8 <- ggplot(dados, aes(x = group, y = `CD80+ CD11c+ CD21- Double Negative B Cells :in: CD11c+ CD21- Double Negative B Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") + # Adiciona bordas aos violinos
  geom_jitter(width = 0.2, size = 1, shape = 21, fill = "black", color = "black", alpha = 0.6) + # Ajusta os pontos
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + # Adiciona linhas de média
  labs(
    title = "DN2 B cells",
    x = "",
    y = "% CD80+"
  ) +
  #scale_y_continuous(limits = c(180, 275)) + # Ajusta os limites do eixo Y
  scale_fill_manual(
    values = fill_d2_d
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", # Remove a legenda
    plot.title = element_text(hjust = 0.5,size=8), # Centraliza o título
    axis.text.x = element_text(size = 6, face = "bold"), # Ajusta o texto do eixo X
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )

plot9 <- ggplot(dados, aes(x = group, y = `CD40+ Non-Classical Monocytes :in: Non-Classical Monocytes`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") + # Adiciona bordas aos violinos
  geom_jitter(width = 0.2, size = 1, shape = 21, fill = "black", color = "black", alpha = 0.6) + # Ajusta os pontos
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + # Adiciona linhas de média
  labs(
    title = "Non-Classical Monocytes",
    x = "",
    y = "% CD40+"
  ) +
  #scale_y_continuous(limits = c(180, 275)) + # Ajusta os limites do eixo Y
  scale_fill_manual(
    values = fill_d2_d
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", # Remove a legenda
    plot.title = element_text(hjust = 0.5,size=8), # Centraliza o título
    axis.text.x = element_text(size = 6, face = "bold"), # Ajusta o texto do eixo X
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )
plot10 <- ggplot(dados, aes(x = group, y = `CD38+ Monocytes :in: Monocytes`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") + # Adiciona bordas aos violinos
  geom_jitter(width = 0.2, size = 1, shape = 21, fill = "black", color = "black", alpha = 0.6) + # Ajusta os pontos
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + # Adiciona linhas de média
  labs(
    title = " Monocytes",
    x = "",
    y = "% CD38+"
  ) +
  #scale_y_continuous(limits = c(180, 275)) + # Ajusta os limites do eixo Y
  scale_fill_manual(
    values = fill_d2_d
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", # Remove a legenda
    plot.title = element_text(hjust = 0.5,size=8), # Centraliza o título
    axis.text.x = element_text(size = 6, face = "bold"), # Ajusta o texto do eixo X
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )
row1 <- plot_grid(plot1, plot2, plot3, plot4, plot5, ncol = 5, align = "hv")
row2 <- plot_grid(plot6, plot7, plot8, plot9, plot10, ncol = 5, align = "hv")

# Combine as linhas verticalmente
combined_plot <- plot_grid(row1, row2, ncol = 1, align = "v")
svg("fig_violin_d2_d.svg", width = 14, height = 5)
print(combined_plot)
dev.off()
combined_plot


## d3_p
dados <- read_csv("./data_aux/fig_violin_d3_p.csv")
dados$group = as.factor(dados$group)
levels(dados$group) = c("Positive","Control","Non-Positive")
dados$group <- factor(dados$group, levels = c("Control", "Non-Positive", "Positive"))

plot1 <- ggplot(dados, aes(x = group, y = `HLA-DR+ median  :in:  HLA-DR+ Double Negative T Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "HLA-DR+ DN T Cells",
    x = "",
    y = "MFI HLA-DR"
  ) +
  scale_fill_manual(
    values =  fill_d3_p
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot2 <- ggplot(dados, aes(x = group, y = `HLA-DR+ median  :in:  HLA-DR+ CD4+ T Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "HLA-DR+ CD4+ T Cells",
    x = "",
    y = "MFI HLA-DR"
  ) +
  scale_fill_manual(
    values =  fill_d3_p
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot3 <- ggplot(dados, aes(x = group, y = `CD11c+ median  :in:  CD11c+ Double Negative T Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "CD11c+ DN T Cells",
    x = "",
    y = "MFI CD11"
  ) +
  scale_fill_manual(
    values =  fill_d3_p
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot4 <- ggplot(dados, aes(x = group, y = `CD57+ median  :in:  CD57+ CD27+ CD8+ T Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "CD57+ CD27+ CD8+ T Cells",
    x = "",
    y = "MFI CD57"
  ) +
  scale_fill_manual(
    values =  fill_d3_p
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot5 <- ggplot(dados, aes(x = group, y = `CD27+ Double Negative T Cells :in: Double Negative T Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "DN T Cells",
    x = "",
    y = "% CD27+"
  ) +
  scale_fill_manual(
    values =  fill_d3_p
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot6 <- ggplot(dados, aes(x = group, y = `Non-Classical Monocytes :in: Monocytes`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "Monocytes",
    x = "",
    y = "% Non-Classical Monocytes"
  ) +
  scale_fill_manual(
    values =  fill_d3_p
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot7 <- ggplot(dados, aes(x = group, y = `HLA-DR+ median  :in:  HLA-DR+ CD27- CD4+ T Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "HLA-DR+ CD27- CD4+ T Cells",
    x = "",
    y = "MFI HLA-DR"
  ) +
  scale_fill_manual(
    values =  fill_d3_p
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot8 <- ggplot(dados, aes(x = group, y = `CD40+ Plasmacytoid Dendritic Cells :in: Plasmacytoid Dendritic Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "Plasmacytoid Dendritic Cells",
    x = "",
    y = "% CD40+"
  ) +
  scale_fill_manual(
    values =  fill_d3_p
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot9 <- ggplot(dados, aes(x = group, y = `CD40+ median  :in:  CD40+ CD21- CD11c+ B Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "CD40+ CD11c+ CD21- B cells",
    x = "",
    y = "MFI CD40"
  ) +
  scale_fill_manual(
    values =  fill_d3_p
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot10 <- ggplot(dados, aes(x = group, y = `CD40+ median  :in:  CD40+ CD11c+ CD21- Double Negative B Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "CD40+ DN2 B cells",
    x = "",
    y = "MFI CD40"
  ) +
  scale_fill_manual(
    values =  fill_d3_p
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

row1 <- plot_grid(plot1, plot2, plot3, plot4, plot5, ncol = 5, align = "hv")
row2 <- plot_grid(plot6, plot7, plot8, plot9, plot10, ncol = 5, align = "hv")
combined_plot <- plot_grid(row1, row2, ncol = 1, align = "v")
svg("fig_violin_d3_p.svg", width = 14, height = 5)
print(combined_plot)
dev.off()
combined_plot

## d3_detecble

dados <- read_csv("./data_aux/fig_violin_d3_detecble.csv")
dados$group = as.factor(dados$group)
levels(dados$group) = c("Detectable","Control","Undetectable")
dados$group <- factor(dados$group, levels = c("Control", "Undetectable", "Detectable"))

plot1 <- ggplot(dados, aes(x = group, y = `CD80+ median  :in:  CD80+ CD11c- CD21+ Double Negative B Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "CD80+ CD11c- CD21+ Double Negative B Cells",
    x = "",
    y = "MFI CD80"
  ) +
  scale_fill_manual(
    values =  fill_d3_detecble
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot2 <- ggplot(dados, aes(x = group, y = `CD27+ CD16+ NK Cells :in: CD16+ NK Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "CD16+ NK Cells",
    x = "",
    y = "% CD27+"
  ) +
  scale_fill_manual(
    values =  fill_d3_detecble
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot3 <- ggplot(dados, aes(x = group, y = `CD40+ median  :in:  CD40+ Intermediate Monocytes`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "CD40+ Intermediate Monocytes",
    x = "",
    y = "MFI CD40"
  ) +
  scale_fill_manual(
    values =  fill_d3_detecble
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot4 <- ggplot(dados, aes(x = group, y = `CD40+ median  :in:  CD40+ Non-Classical Monocytes`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "CD40+ Non-Classical Monocytes",
    x = "",
    y = "MFI CD40"
  ) +
  scale_fill_manual(
    values =  fill_d3_detecble
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot5 <- ggplot(dados, aes(x = group, y = `IgM+ Memory B Cells :in: B Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "B Cells",
    x = "",
    y = "% IgM+ Memory B Cells"
  ) +
  scale_fill_manual(
    values =  fill_d3_detecble
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot6 <- ggplot(dados, aes(x = group, y = `CD16+ CD56dim NK Cells :in: NK Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "NK Cells",
    x = "",
    y = "% CD16+ CD56dim NK Cells"
  ) +
  scale_fill_manual(
    values =  fill_d3_detecble
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot7 <- ggplot(dados, aes(x = group, y = `CD40+ median  :in:  CD40+ Monocytes`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "CD40+ Monocytes",
    x = "",
    y = "MFI CD40"
  ) +
  scale_fill_manual(
    values =  fill_d3_detecble
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot8 <- ggplot(dados, aes(x = group, y = `CD38+ Type 2 Myeloid Dendritic Cells :in: mDC2`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "Type 2 Myeloid Dendritic Cells",
    x = "",
    y = "% CD38+"
  ) +
  scale_fill_manual(
    values =  fill_d3_detecble
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot9 <- ggplot(dados, aes(x = group, y = `CD40+ Monocytes :in: Monocytes`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "Monocytes",
    x = "",
    y = "% CD40+"
  ) +
  scale_fill_manual(
    values =  fill_d3_detecble
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot10 <- ggplot(dados, aes(x = group, y = `CD27+ NK Cells :in: NK Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "NK Cells",
    x = "",
    y = "% CD27+"
  ) +
  scale_fill_manual(
    values =  fill_d3_detecble
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

row1 <- plot_grid(plot1, plot2, plot3, plot4, plot5, ncol = 5, align = "hv")
row2 <- plot_grid(plot6, plot7, plot8, plot9, plot10, ncol = 5, align = "hv")
combined_plot <- plot_grid(row1, row2, ncol = 1, align = "v")
svg("fig_violin_d3_detecble.svg", width = 14, height = 5)
print(combined_plot)
dev.off()
combined_plot

### d3_depos
dados <- read_csv("./data_aux/fig_violin_d3_depos.csv")
dados$group = as.factor(dados$group)
levels(dados$group) = c("Detectable","Control","Positive")
dados$group <- factor(dados$group, levels = c("Control", "Detectable", "Positive"))

plot1 <- ggplot(dados, aes(x = group, y = `CD27+ NK Cells :in: NK Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "NK Cells",
    x = "",
    y = "% CD27+"
  ) +
  scale_fill_manual(
    values =  fill_d3_depos
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot2 <- ggplot(dados, aes(x = group, y = `CD40+ median  :in:  CD40+ Intermediate Monocytes`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "CD40+ Intermediate Monocytes",
    x = "",
    y = "MFI CD40"
  ) +
  scale_fill_manual(
    values =  fill_d3_depos
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot3 <- ggplot(dados, aes(x = group, y = `CD27+ CD16+ NK Cells :in: CD16+ NK Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "CD16+ NK Cells",
    x = "",
    y = "% CD27+"
  ) +
  scale_fill_manual(
    values =  fill_d3_depos
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot4 <- ggplot(dados, aes(x = group, y = `CD57+ median  :in:  CD57+ CD27+ CD8+ T Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "CD57+ CD27+ CD8+ T Cells",
    x = "",
    y = "MFI CD57"
  ) +
  scale_fill_manual(
    values =  fill_d3_depos
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot5 <- ggplot(dados, aes(x = group, y = `HLA-DR+ median  :in:  HLA-DR+ CD4+ T Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "HLA-DR+ CD4+ T Cells",
    x = "",
    y = "MFI HLA-DR"
  ) +
  scale_fill_manual(
    values =  fill_d3_depos
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot6 <- ggplot(dados, aes(x = group, y = `CD11c+ CD27- CD4+ T Cells :in: CD27- CD4+ T Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "CD27- CD4+ T Cells",
    x = "",
    y = "% CD11c+"
  ) +
  scale_fill_manual(
    values =  fill_d3_depos
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot7 <- ggplot(dados, aes(x = group, y = `CD57+ CD27+ CD8+ T Cells :in: CD27+ CD8+ T Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "CD27+ CD8+ T Cells",
    x = "",
    y = "% CD57+"
  ) +
  scale_fill_manual(
    values =  fill_d3_depos
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot8 <- ggplot(dados, aes(x = group, y = `CD27- CD4+ T Cells :in: CD4+ T Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "CD4+ T Cells",
    x = "",
    y = "% CD27-"
  ) +
  scale_fill_manual(
    values =  fill_d3_depos
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot9 <- ggplot(dados, aes(x = group, y = `CD27+ CD4+ T Cells :in: CD4+ T Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "CD4+ T Cells",
    x = "",
    y = "% CD27+"
  ) +
  scale_fill_manual(
    values =  fill_d3_depos
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

plot10 <- ggplot(dados, aes(x = group, y = `CD80+ IgMlow Memory B Cells :in: IgMlow Memory B Cells`, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.15, size = 1, shape = 21, fill = "black", color = "black") + 
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "darkgray", size = 0.3) + 
  labs(
    title = "IgMlow Memory B Cells",
    x = "",
    y = "% CD80+"
  ) +
  scale_fill_manual(
    values =  fill_d3_depos
  ) +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5,size=8), 
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )+
  guides(fill = "none")

row1 <- plot_grid(plot1, plot2, plot3, plot4, plot5, ncol = 5, align = "hv")
row2 <- plot_grid(plot6, plot7, plot8, plot9, plot10, ncol = 5, align = "hv")
combined_plot <- plot_grid(row1, row2, ncol = 1, align = "v")
svg("fig_violin_d3_depos.svg", width = 14, height = 5)
print(combined_plot)
dev.off()
combined_plot

################################################################################################
##d2_d

df = read.csv("data_aux/heat_d2_d.csv") #

f = file("data_aux/heat_d2_d.csv","r") #
lin = readLines(f,1)
close(f)
lin = unlist(strsplit(lin, ","))
col_label = lin[5:14]

normal = df[df$gr_d2_d=="h",] #
normal$Sample = NULL
normal$batch = NULL
normal$type = NULL
normal$gr_d2_d = NULL #
c_nor = f_cor(normal)

detec = df[df$gr_d2_d=="d",] #
detec$Sample = NULL
detec$batch = NULL
detec$type = NULL
detec$gr_d2_d = NULL #
c_detec = f_cor(detec)
comp_detect = f_comp(c_nor,c_detec)

ndetec = df[df$gr_d2_d=="n",] #
ndetec$Sample = NULL
ndetec$batch = NULL
ndetec$type = NULL
ndetec$gr_d2_d = NULL #
c_ndetec = f_cor(ndetec)
comp_ndetect = f_comp(c_nor,c_ndetec)

dend = as.dendrogram(hclust(dist(abs(c_nor))))
dend = color_branches(dend, k = 3)

write.csv(comp_detect, "data_aux/comp_detect_d2_d.csv", row.names=FALSE) #
write.csv(comp_ndetect, "data_aux/comp_ndetect_d2_d.csv", row.names=FALSE) #


## heathy
h = Heatmap(f_diagonal(c_nor),name="Correlation",col = col_fun,
            column_title = "Heathy",
            column_title_side = "top",
            show_row_dend = FALSE,
            row_labels = gt_render(col_label),
            row_names_gp = grid::gpar(fontsize = 10),
            cluster_rows = dend,
            row_names_side = "left",
            column_labels = gt_render(col_label),
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
            column_title = "Dose 2 detectable", #
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
            column_title = "Dose 2 non-responders", #
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
svg("fig_heat_d2_d.svg", width = 20.9, height = 10.5) #
lgd = Legend(col_fun = colormap2, title = "Healthy",at = c(2, 0, -2),labels = c("low", "equal", "high"))
B =h+d+n

draw(B,heatmap_legend_side = "right",column_title = "",column_title_gp = gpar(fontsize = 16))
draw(lgd, x = unit(43, "cm"), y = unit(12.6, "cm"), just = c("right", "top"))
dev.off()

##d3_p

df = read.csv("data_aux/heat_d3_p.csv") #

f = file("data_aux/heat_d3_p.csv","r") #
lin = readLines(f,1)
close(f)
lin = unlist(strsplit(lin, ","))
col_label = lin[5:14]

normal = df[df$gr_d3_p=="h",] #
normal$Sample = NULL
normal$batch = NULL
normal$type = NULL
normal$gr_d3_p = NULL #
c_nor = f_cor(normal)

detec = df[df$gr_d3_p=="d",] #
detec$Sample = NULL
detec$batch = NULL
detec$type = NULL
detec$gr_d3_p = NULL #
c_detec = f_cor(detec)
comp_detect = f_comp(c_nor,c_detec)

ndetec = df[df$gr_d3_p=="n",] #
ndetec$Sample = NULL
ndetec$batch = NULL
ndetec$type = NULL
ndetec$gr_d3_p = NULL #
c_ndetec = f_cor(ndetec)
comp_ndetect = f_comp(c_nor,c_ndetec)

dend = as.dendrogram(hclust(dist(abs(c_nor))))
dend = color_branches(dend, k = 3)

write.csv(comp_detect, "data_aux/comp_detect_d3_p.csv", row.names=FALSE) #
write.csv(comp_ndetect, "data_aux/comp_ndetect_d3_p.csv", row.names=FALSE) #


## heathy
h = Heatmap(f_diagonal(c_nor),name="Correlation",col = col_fun,
            column_title = "Heathy",
            column_title_side = "top",
            show_row_dend = FALSE,
            row_labels = gt_render(col_label),
            row_names_gp = grid::gpar(fontsize = 10),
            cluster_rows = dend,
            row_names_side = "left",
            column_labels = gt_render(col_label),
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
            column_title = "Dose 3 positive", #
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
            column_title = "Dose 3 non-positive", #
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
svg("fig_heat_d3_p.svg", width = 20.9, height = 10.5) #
lgd = Legend(col_fun = colormap2, title = "Healthy",at = c(2, 0, -2),labels = c("low", "equal", "high"))
B =h+d+n

draw(B,heatmap_legend_side = "right",column_title = "",column_title_gp = gpar(fontsize = 16))
draw(lgd, x = unit(43, "cm"), y = unit(12.6, "cm"), just = c("right", "top"))
dev.off()

##d3_detecble

df = read.csv("data_aux/heat_d3_detecble.csv") #

f = file("data_aux/heat_d3_detecble.csv","r") #
lin = readLines(f,1)
close(f)
lin = unlist(strsplit(lin, ","))
col_label = lin[5:14]

normal = df[df$gr_d3_detecble=="h",] #
normal$Sample = NULL
normal$batch = NULL
normal$type = NULL
normal$gr_d3_detecble = NULL #
c_nor = f_cor(normal)

detec = df[df$gr_d3_detecble=="d",] #
detec$Sample = NULL
detec$batch = NULL
detec$type = NULL
detec$gr_d3_detecble = NULL #
c_detec = f_cor(detec)
comp_detect = f_comp(c_nor,c_detec)

ndetec = df[df$gr_d3_detecble=="n",] #
ndetec$Sample = NULL
ndetec$batch = NULL
ndetec$type = NULL
ndetec$gr_d3_detecble = NULL #
c_ndetec = f_cor(ndetec)
comp_ndetect = f_comp(c_nor,c_ndetec)

dend = as.dendrogram(hclust(dist(abs(c_nor))))
dend = color_branches(dend, k = 3)

write.csv(comp_detect, "data_aux/comp_detect_d3_detecble.csv", row.names=FALSE) #
write.csv(comp_ndetect, "data_aux/comp_ndetect_d3_detecble.csv", row.names=FALSE) #


## heathy
h = Heatmap(f_diagonal(c_nor),name="Correlation",col = col_fun,
            column_title = "Heathy",
            column_title_side = "top",
            show_row_dend = FALSE,
            row_labels = gt_render(col_label),
            row_names_gp = grid::gpar(fontsize = 10),
            cluster_rows = dend,
            row_names_side = "left",
            column_labels = gt_render(col_label),
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
            column_title = "Dose 3 detectable non-positive", #
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
            column_title = "Dose 3 non-responders", #
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
svg("fig_heat_d3_detecble.svg", width = 20.9, height = 10.5) #
lgd = Legend(col_fun = colormap2, title = "Healthy",at = c(2, 0, -2),labels = c("low", "equal", "high"))
B =h+d+n

draw(B,heatmap_legend_side = "right",column_title = "",column_title_gp = gpar(fontsize = 16))
draw(lgd, x = unit(43, "cm"), y = unit(12.6, "cm"), just = c("right", "top"))
dev.off()

##d3_depos

df = read.csv("data_aux/heat_d3_depos.csv") #

f = file("data_aux/heat_d3_depos.csv","r") #
lin = readLines(f,1)
close(f)
lin = unlist(strsplit(lin, ","))
col_label = lin[5:14]
df$gr_d3_depos[df$gr_d3_depos=="d"] = "n"
df$gr_d3_depos[df$gr_d3_depos=="p"] = "d"

normal = df[df$gr_d3_depos=="h",] #
normal$Sample = NULL
normal$batch = NULL
normal$type = NULL
normal$gr_d3_depos = NULL #
c_nor = f_cor(normal)

detec = df[df$gr_d3_depos=="d",] #
detec$Sample = NULL
detec$batch = NULL
detec$type = NULL
detec$gr_d3_depos = NULL #
c_detec = f_cor(detec)
comp_detect = f_comp(c_nor,c_detec)

ndetec = df[df$gr_d3_depos=="n",] #
ndetec$Sample = NULL
ndetec$batch = NULL
ndetec$type = NULL
ndetec$gr_d3_depos = NULL #
c_ndetec = f_cor(ndetec)
comp_ndetect = f_comp(c_nor,c_ndetec)

dend = as.dendrogram(hclust(dist(abs(c_nor))))
dend = color_branches(dend, k = 3)

write.csv(comp_detect, "data_aux/comp_detect_d3_depos.csv", row.names=FALSE) #
write.csv(comp_ndetect, "data_aux/comp_ndetect_d3_depos.csv", row.names=FALSE) #


## heathy
h = Heatmap(f_diagonal(c_nor),name="Correlation",col = col_fun,
            column_title = "Heathy",
            column_title_side = "top",
            show_row_dend = FALSE,
            row_labels = gt_render(col_label),
            row_names_gp = grid::gpar(fontsize = 10),
            cluster_rows = dend,
            row_names_side = "left",
            column_labels = gt_render(col_label),
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
            column_title = "Dose 3 positive", #
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
            column_title = "Dose 3 detectable", #
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
svg("fig_heat_d3_depos.svg", width = 20.9, height = 10.5) #
lgd = Legend(col_fun = colormap2, title = "Healthy",at = c(2, 0, -2),labels = c("low", "equal", "high"))
B =h+d+n

draw(B,heatmap_legend_side = "right",column_title = "",column_title_gp = gpar(fontsize = 16))
draw(lgd, x = unit(43, "cm"), y = unit(12.6, "cm"), just = c("right", "top"))
dev.off()

################################################################################
# Suplementary fig 2
data = read_csv("./data_aux/sup2.csv")

# Mapeie: X = grupo, Y = variável contínua, fill = grupo (para as cores),
# e use 'group = interaction(grupo, MMF)' e 'position_dodge' 
# para obter dois violinos (No/Yes) por grupo.
data$d3_group <- factor(data$d3_group, levels = c("Control", "Non-Positive", "Positive"))
data$MMF      <- factor(data$MMF,      levels = c("No", "Yes"))

library(tidyverse)
library(ggsignif)  # para geom_signif()

make_violin_plot <- function(df, var, ylab, titulo, esp) {
  df_sub <- df %>%
    select(d3_group, MMF, all_of(var)) %>%
    mutate(value = .data[[var]])
  
  df_max <- df_sub %>%
    group_by(d3_group) %>%
    summarise(maxY = max(value, na.rm = TRUE), .groups = "drop")
  
  compare_df <- df_sub %>%
    filter(d3_group != "Control") %>%
    group_by(d3_group) %>%
    filter(n_distinct(MMF) == 2) %>%
    summarise(p.value = wilcox.test(value ~ MMF)$p.value, .groups = "drop") %>%
    mutate(
      p.signif = case_when(
        p.value < 0.0001 ~ "****",
        p.value < 0.001  ~ "***",
        p.value < 0.01   ~ "**",
        p.value < 0.05   ~ "*",
        TRUE             ~ "ns"
      )
    ) %>%
    left_join(df_max, by = "d3_group") %>%
    mutate(
      x_group    = as.numeric(d3_group),
      xmin       = x_group - 0.2,
      xmax       = x_group + 0.2,
      y_position = maxY * 1.10
    ) %>%
    filter(p.value < 0.05)
  
  p <- ggplot(df_sub, aes(x = d3_group, y = value, fill = d3_group)) +
    geom_violin(
      aes(group = interaction(d3_group, MMF)),
      position = position_dodge(width = 0.8),
      width    = esp,
      trim     = FALSE,
      color    = "black",
      adjust   = 2.5
    ) +
    geom_jitter(
      aes(shape = MMF),
      position = position_jitterdodge(
        dodge.width   = 0.8,
        jitter.width  = 0.6,
        jitter.height = 0
      ),
      size  = 1,
      color = "black"
    ) +
    stat_summary(
      aes(group = interaction(d3_group, MMF)),
      fun      = median,
      geom     = "crossbar",
      color    = "black",
      width    = 0.3,
      position = position_dodge(width = 0.8)
    ) +
    geom_signif(
      data        = compare_df,
      aes(
        xmin       = xmin,
        xmax       = xmax,
        annotations= p.signif,
        y_position = y_position
      ),
      manual      = TRUE,
      inherit.aes = FALSE,
      tip_length  = 0.01,
      textsize    = 4,
      size        = 0.5
    ) +
    scale_fill_manual(values = c(
      "Control"      = "#7f7f7f",
      "Non-Positive" = "#ff7f0e",
      "Positive"     = "#1f77b4"
    )) +
    scale_shape_manual(values = c("No" = 16, "Yes" = 4)) +
    theme_classic(base_size = 12) +
    theme(
      panel.border    = element_rect(color = "black", fill = NA),
      legend.position = "right",
      axis.text.x     = element_text(size = 8),
      axis.text.y     = element_text(size = 8),
      axis.title.x    = element_text(size = 10),
      axis.title.y    = element_text(size = 10),
      plot.title      = element_text(size = 10)
    ) +
    guides(fill = "none") +
    labs(
      title = titulo,
      x     = NULL,
      y     = ylab,
      shape = "MMF"
    )
  
  if (nrow(compare_df) > 0) {
    p <- p + expand_limits(y = max(compare_df$y_position) * 1.05)
  }
  
  return(p)
}

data <- data %>% filter(!is.na(d3_group))
df <- data %>% filter(!is.na(`HLA-DR+ median  :in:  HLA-DR+ Double Negative T Cells`))

p1 <- make_violin_plot(df, 
                       var   = "HLA-DR+ median  :in:  HLA-DR+ Double Negative T Cells", 
                       ylab  = "MFI HLA-DR", 
                       titulo= "HLA-DR+ DN T Cells",1)

df <- data %>% filter(!is.na(`HLA-DR+ median  :in:  HLA-DR+ CD4+ T Cells`))
p2 <- make_violin_plot(df, 
                       var   = "HLA-DR+ median  :in:  HLA-DR+ CD4+ T Cells", 
                       ylab  = "MFI HLA-DR", 
                       titulo= "HLA-DR+ CD4+ T Cells",1)

df <- data %>% filter(!is.na(`CD11c+ median  :in:  CD11c+ Double Negative T Cells`))
p3 <- make_violin_plot(df, 
                       var   = "CD11c+ median  :in:  CD11c+ Double Negative T Cells", 
                       ylab  = "MFI CD11c", 
                       titulo= "CD11c+ DN T Cells",1)

df <- data %>% filter(!is.na(`CD57+ median  :in:  CD57+ CD27+ CD8+ T Cells`))
p4 <- make_violin_plot(df, 
                       var   = "CD57+ median  :in:  CD57+ CD27+ CD8+ T Cells", 
                       ylab  = "MFI CD57", 
                       titulo= "CD57+ CD27+ CD8+ T Cells",1)
var = "CD27+ Double Negative T Cells :in: Double Negative T Cells"
df <- data %>% filter(!is.na(var))
p5 <- make_violin_plot(df, 
                       var   = var, 
                       ylab  = "% CD27+", 
                       titulo= "DN T Cells",1)

var = "Non-Classical Monocytes :in: Monocytes"
df <- data %>% filter(!is.na(var))
p6 <- make_violin_plot(df, 
                       var   = var, 
                       ylab  = "% Non-Classical Monocytes", 
                       titulo= "Monocytes",1)

var = "HLA-DR+ median  :in:  HLA-DR+ CD27- CD4+ T Cells"
df <- data %>% filter(!is.na(var))
p7 <- make_violin_plot(df, 
                       var   = var, 
                       ylab  = "MFI HLA-DR", 
                       titulo= "HLA-DR+ CD27- CD4+ T Cells",1)

var = "CD40+ Plasmacytoid Dendritic Cells :in: Plasmacytoid Dendritic Cells"
df <- data %>% filter(!is.na(var))
p8 <- make_violin_plot(df, 
                       var   = var, 
                       ylab  = "% CD40+", 
                       titulo= "pDCs",1)

var = "CD40+ median  :in:  CD40+ CD21- CD11c+ B Cells"
df <- data %>% filter(!is.na(var))
p9 <- make_violin_plot(df, 
                       var   = var, 
                       ylab  = "MFI CD40", 
                       titulo= "CD40+ CD21- CD11c+ B Cells",1)

var = "CD40+ median  :in:  CD40+ CD11c+ CD21- Double Negative B Cells"
df <- data %>% filter(!is.na(var))
p10 <- make_violin_plot(df, 
                       var   = var, 
                       ylab  = "MFI CD40", 
                       titulo= "CD40+ DN2 B Cells",1)

final_figure <- ggarrange(
  p1, p2, p3, p4, p5,
  p6, p7, p8, p9, p10,
  ncol = 5, nrow = 2,
  common.legend = TRUE,  # usa a mesma legenda (para "MMF")
  legend = "right"
)
svg("sup2.svg", width = 14.9, height = 5.5)
final_figure
dev.off()

################################################################################
# Suplementary fig 3
data = read_csv("./data_aux/sup3.csv")

# Mapeie: X = grupo, Y = variável contínua, fill = grupo (para as cores),
# e use 'group = interaction(grupo, MMF)' e 'position_dodge' 
# para obter dois violinos (No/Yes) por grupo.
data$d3_group <- factor(data$d3_group, levels = c("Control", "Non-Positive", "Positive"))
data$CS      <- factor(data$CS,      levels = c("No", "Yes"))

library(tidyverse)
library(ggsignif)  # para geom_signif()

make_violin_plot <- function(df, var, ylab, titulo, esp) {
  df_sub <- df %>%
    select(d3_group, CS, all_of(var)) %>%
    mutate(value = .data[[var]])
  
  df_max <- df_sub %>%
    group_by(d3_group) %>%
    summarise(maxY = max(value, na.rm = TRUE), .groups = "drop")
  
  compare_df <- df_sub %>%
    filter(d3_group != "Control") %>%
    group_by(d3_group) %>%
    filter(n_distinct(CS) == 2) %>%
    summarise(p.value = wilcox.test(value ~ CS)$p.value, .groups = "drop") %>%
    mutate(
      p.signif = case_when(
        p.value < 0.0001 ~ "****",
        p.value < 0.001  ~ "***",
        p.value < 0.01   ~ "**",
        p.value < 0.05   ~ "*",
        TRUE             ~ "ns"
      )
    ) %>%
    left_join(df_max, by = "d3_group") %>%
    mutate(
      x_group    = as.numeric(d3_group),
      xmin       = x_group - 0.2,
      xmax       = x_group + 0.2,
      y_position = maxY * 1.10
    ) %>%
    filter(p.value < 0.05)
  
  p <- ggplot(df_sub, aes(x = d3_group, y = value, fill = d3_group)) +
    geom_violin(
      aes(group = interaction(d3_group, CS)),
      position = position_dodge(width = 0.8),
      width    = esp,
      trim     = FALSE,
      color    = "black",
      adjust   = 2.5
    ) +
    geom_jitter(
      aes(shape = CS),
      position = position_jitterdodge(
        dodge.width   = 0.8,
        jitter.width  = 0.6,
        jitter.height = 0
      ),
      size  = 1,
      color = "black"
    ) +
    stat_summary(
      aes(group = interaction(d3_group, CS)),
      fun      = median,
      geom     = "crossbar",
      color    = "black",
      width    = 0.3,
      position = position_dodge(width = 0.8)
    ) +
    geom_signif(
      data        = compare_df,
      aes(
        xmin       = xmin,
        xmax       = xmax,
        annotations= p.signif,
        y_position = y_position
      ),
      manual      = TRUE,
      inherit.aes = FALSE,
      tip_length  = 0.01,
      textsize    = 4,
      size        = 0.5
    ) +
    scale_fill_manual(values = c(
      "Control"      = "#7f7f7f",
      "Non-Positive" = "#ff7f0e",
      "Positive"     = "#1f77b4"
    )) +
    scale_shape_manual(values = c("No" = 16, "Yes" = 4)) +
    theme_classic(base_size = 12) +
    theme(
      panel.border    = element_rect(color = "black", fill = NA),
      legend.position = "right",
      axis.text.x     = element_text(size = 8),
      axis.text.y     = element_text(size = 8),
      axis.title.x    = element_text(size = 10),
      axis.title.y    = element_text(size = 10),
      plot.title      = element_text(size = 10)
    ) +
    guides(fill = "none") +
    labs(
      title = titulo,
      x     = NULL,
      y     = ylab,
      shape = "CS"
    )
  
  if (nrow(compare_df) > 0) {
    p <- p + expand_limits(y = max(compare_df$y_position) * 1.05)
  }
  
  return(p)
}

data <- data %>% filter(!is.na(d3_group))
df <- data %>% filter(!is.na(`HLA-DR+ median  :in:  HLA-DR+ Double Negative T Cells`))

p1 <- make_violin_plot(df, 
                       var   = "HLA-DR+ median  :in:  HLA-DR+ Double Negative T Cells", 
                       ylab  = "MFI HLA-DR", 
                       titulo= "HLA-DR+ DN T Cells",1)

df <- data %>% filter(!is.na(`HLA-DR+ median  :in:  HLA-DR+ CD4+ T Cells`))
p2 <- make_violin_plot(df, 
                       var   = "HLA-DR+ median  :in:  HLA-DR+ CD4+ T Cells", 
                       ylab  = "MFI HLA-DR", 
                       titulo= "HLA-DR+ CD4+ T Cells",1)

df <- data %>% filter(!is.na(`CD11c+ median  :in:  CD11c+ Double Negative T Cells`))
p3 <- make_violin_plot(df, 
                       var   = "CD11c+ median  :in:  CD11c+ Double Negative T Cells", 
                       ylab  = "MFI CD11c", 
                       titulo= "CD11c+ DN T Cells",1)

df <- data %>% filter(!is.na(`CD57+ median  :in:  CD57+ CD27+ CD8+ T Cells`))
p4 <- make_violin_plot(df, 
                       var   = "CD57+ median  :in:  CD57+ CD27+ CD8+ T Cells", 
                       ylab  = "MFI CD57", 
                       titulo= "CD57+ CD27+ CD8+ T Cells",1)
var = "CD27+ Double Negative T Cells :in: Double Negative T Cells"
df <- data %>% filter(!is.na(var))
p5 <- make_violin_plot(df, 
                       var   = var, 
                       ylab  = "% CD27+", 
                       titulo= "DN T Cells",1)

var = "Non-Classical Monocytes :in: Monocytes"
df <- data %>% filter(!is.na(var))
p6 <- make_violin_plot(df, 
                       var   = var, 
                       ylab  = "% Non-Classical Monocytes", 
                       titulo= "Monocytes",1)

var = "HLA-DR+ median  :in:  HLA-DR+ CD27- CD4+ T Cells"
df <- data %>% filter(!is.na(var))
p7 <- make_violin_plot(df, 
                       var   = var, 
                       ylab  = "MFI HLA-DR", 
                       titulo= "HLA-DR+ CD27- CD4+ T Cells",1)

var = "CD40+ Plasmacytoid Dendritic Cells :in: Plasmacytoid Dendritic Cells"
df <- data %>% filter(!is.na(var))
p8 <- make_violin_plot(df, 
                       var   = var, 
                       ylab  = "% CD40+", 
                       titulo= "pDCs",1)

var = "CD40+ median  :in:  CD40+ CD21- CD11c+ B Cells"
df <- data %>% filter(!is.na(var))
p9 <- make_violin_plot(df, 
                       var   = var, 
                       ylab  = "MFI CD40", 
                       titulo= "CD40+ CD21- CD11c+ B Cells",1)

var = "CD40+ median  :in:  CD40+ CD11c+ CD21- Double Negative B Cells"
df <- data %>% filter(!is.na(var))
p10 <- make_violin_plot(df, 
                        var   = var, 
                        ylab  = "MFI CD40", 
                        titulo= "CD40+ DN2 B Cells",1)

final_figure <- ggarrange(
  p1, p2, p3, p4, p5,
  p6, p7, p8, p9, p10,
  ncol = 5, nrow = 2,
  common.legend = TRUE,  # usa a mesma legenda (para "CS")
  legend = "right"
)
svg("sup3.svg", width = 14.9, height = 5.5)
final_figure
dev.off()