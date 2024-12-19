# Carregar pacotes necessários
library(readr)
library(cowplot)
library(tidyverse)
library(ComplexHeatmap)
library(circlize)
library(gridtext)
library(dendextend)
library(svglite)

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
    title = "CD80+ CD11c- CD21+ Double Negative B Cells",
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
    title = "CD57+ Double Negative T Cells",
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
    title = "BAFF-R+ CD11c- CD21+ Double Negative B Cells",
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
    title = "CD11c+ CD21- Double Negative B Cells",
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
    title = "HLA-DR+ Double Negative T Cells",
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
    title = "CD11c+ Double Negative T Cells",
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
    title = "Double Negative T Cells",
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
    title = "CD40+ CD21- CD11c+ B Cells",
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
    title = "CD40+ CD11c+ CD21- Double Negative B Cells",
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
    y = "MFI CD27+"
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