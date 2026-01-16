library(ggplot2)
library(mgcv)
library(Rmisc)
library(scales)
library(cowplot)
library(ggpubr)
library(dplyr)
library(mgcv)
library(ggpointdensity) # 绘制密度散点图
library(viridis)

#data1 <- read.csv("E:/UTCI/Result2_new/ZoneB/data_B2.csv")
data1 <- read.csv("E:/UTCI/Result2/data_E_final.csv")
gam_poisson <- gam(HW_intensity~s(Tem,k=5),
                   data = data1, method = 'REML')
summary(gam_poisson)

#输出原值和拟合值

y_simu<-fitted(gam_poisson)

df=data.frame(x=data1$Tem,y=data1$HW_intensity,ys=y_simu)

#write.xlsx(df,"tcp的原值PMgam拟合值.xlsx",rowNmes=FALSE)


windowsFonts(A=windowsFont("Times New Roman"))
p1 <- ggplot(df) +
  scale_color_manual(values = c('#e89653',"#535d91")) +
  geom_smooth(aes(x = x, y = y), method = "gam", formula = y ~ s(x, k = 5), se = TRUE,
              color = "SteelBlue", linewidth = 0.7, alpha = 0.3) +
  geom_rug(aes(x = x), sides = "b", length = unit(0.01, "npc")) +  # 调整 rug plot 的密度
  geom_abline(slope = 1, intercept = 0, linewidth = 0.5, linetype = "dashed", color = "gray") +  # 添加 y=x 的辅助线
  theme_bw() +
  geom_hline(yintercept = 0, linewidth = 0.5, color = 'red', linetype = "dashed") +
  xlab("Tem_rate") +
  ylab("平均热胁迫变化率") +
  guides(color = "none") +
  scale_x_continuous(limits = c(-0.08, 0.23), breaks = seq(-0.05, 0.20, by = 0.05)) +  # 设置横坐标轴范围和间隔
  scale_y_continuous(limits = c(-0.08, 0.23), breaks = seq(-0.05, 0.20, by = 0.05)) +  # 设置纵坐标轴范围和间隔
  theme(axis.text = element_text(family = 'Times New Roman',size =13),  # 设置坐标轴标注字体为新罗马
        axis.title.x = element_text(family = 'Times New Roman'),  # 设置横坐标标题字体为新罗马
        axis.title.y = element_text(family = 'SimSun'),  # 设置纵坐标标题字体为宋体
        axis.title = element_text(size = 15),  # 设置坐标轴标题字体大小
        panel.grid.major = element_line(color = "gray"),  # 设置主要网格线
        panel.grid.minor = element_blank(),  # 不显示次要网格线
        panel.border = element_blank(),  # 不显示面板边框
        panel.background = element_blank(),  # 不显示面板背景
        axis.line.x = element_line(color = "black"),  # 设置下轴坐标轴颜色
        axis.line.y = element_line(color = "black"),  # 设置左轴坐标轴颜色
        axis.ticks = element_line(color = "black")) +  # 设置坐标轴刻度线颜色
  theme(legend.position = "none")  # 不显示图例
ggsave("E:/UTCI2_figure/figure5.png", p1, dpi = 600)   