##绘制前后二十年变化
library(raster)
library(ggplot2)
library(sf)
library(maps)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(Cairo)
library(png)
library(hatch)

# 读取coast shapefile数据
coast <- st_read("E:/UTCI2_figure/Chapter2.2/Data2.2.1/bianjie.shp")

# 读取TIFF文件
raster_data <- raster("E:/UTCI2_figure/Chapter2.2/Slope_clip/sen_intensity_mean20_2_process.tif")
sig <- st_read("E:/UTCI2_figure/Chapter2.2/Slope_clip/mk_intensity_mean20_2_process.shp")
#raster_data3 <- raster("E:/UTCI2_figure/Chapter2.2/Slope_clip/sen_day_mean20_2.tif")
#raster_data4 <- raster("E:/UTCI2_figure/Chapter2.2/Slope_clip/mk_day_mean20_2.tif")

# 设置经纬度范围
extent(raster_data) <- c(-180, 180, -60, 90)

# 将栅格数据转换为data.frame
raster_df <- as.data.frame(raster_data, xy = TRUE)
raster_df <- na.omit(raster_df)

#色带设置
custom_color_gradient <- colorRampPalette(c(rgb(56, 146, 34, maxColorValue = 255),
                                            rgb(209, 232, 153, maxColorValue = 255),
                                            rgb(255, 254, 190, maxColorValue = 255),
                                            rgb(245, 191, 135, maxColorValue = 255),
                                            rgb(227, 123, 81, maxColorValue = 255),
                                            rgb(167, 33, 23, maxColorValue = 255)))

custom_colors <- custom_color_gradient(10)  # 7代表颜色分段数
# 使用 ggplot2 和 geom_raster() 进行绘图

p1 <- ggplot() +
  geom_raster(data = raster_df, aes(x = x, y = y, fill = sen_intensity_mean20_1_process), na.value = 'transparent') +
  scale_fill_stepsn(breaks = c(-0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06, 0.08, 0.1),  # 设置图例的间隔为每60
                    colors = custom_colors,  # 设置分段颜色
                    guide = guide_colorbar(  # 设置图例
                      direction = "horizontal",
                      nrow = 1,
                      title.position = "right",
                      barwidth = 20,
                      barheight = 0.7,
                      ticks.colour = NA,
                      ticks.linewidth = 0.3,
                      title = element_blank(),
                      text = element_text(family = "serif")  
                    )
  ) +  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) +
  scale_x_continuous(breaks = c(-179.8, -120, -60, 0, 60, 120, 179.8),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-59.9, -30, 0, 30, 60, 89.9),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N", "90°N")) +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude") +
  theme_linedraw() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),  
    panel.background = element_rect(fill = "white", color = NA),  
    panel.border = element_rect(size = 0.5, color = "black"), 
    axis.title = element_blank(),
    axis.text = element_text(size = 7),
    axis.ticks = element_line(),
    axis.ticks.length = unit(0.15, "cm"), 
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "white"),  
    panel.grid.minor = element_blank(),
    plot.margin = margin(1, 1, 1, 1, "cm"),  # 增加右侧边距
    panel.spacing = unit(0, "cm"),
    text = element_text(family = "serif"),
  )+  
  geom_sf(data = coast, fill = "transparent", size = 0.1, color = rgb(117, 116, 113, maxColorValue = 255),lwd=0.1)+
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE)+
  geom_sf(data = sig,size = 0.08,shape=4, stroke=0.05,color = alpha("black", 0.6)) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE)
p1
ggsave("E:/UTCI2_figure/figure4.png", p1, dpi = 600,width = 6,height=4)   