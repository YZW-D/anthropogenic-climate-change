library(lightgbm)
library(pdp)
library(lattice)
library(iml)
library(ggplot2)
library(dplyr)
library(DALEX)

#加载数据集
#data <- read.csv("E:/UTCI/Result2/ZoneE/data_E5.csv")
data1 <- read.csv("E:/UTCI/Result2/data_A_final.csv")
data2 <- read.csv("E:/UTCI/Result2/data_B_final.csv")
data3 <- read.csv("E:/UTCI/Result2/data_C_final.csv")
data4 <- read.csv("E:/UTCI/Result2/data_D_final.csv")
data5 <- read.csv("E:/UTCI/Result2/data_E_final.csv")
data <- rbind(data1,data2,data3,data4,data5)
#data <- read.csv("E:/UTCI/Result2/data_all.csv")
data1 <- read.csv("E:/UTCI/Result2_new/ZoneA/data_A2.csv")
data2 <- read.csv("E:/UTCI/Result2_new/ZoneB/data_B2.csv")
data3 <- read.csv("E:/UTCI/Result2_new/ZoneC/data_C2.csv")
data4 <- read.csv("E:/UTCI/Result2_new/ZoneD/data_D2.csv")
data5 <- read.csv("E:/UTCI/Result2_new/ZoneE/data_E2.csv")
data <- rbind(data1,data2,data3,data4,data5)


#拆分训练集和测试集
set.seed(100)  # 设置随机数种子以确保结果可重现
train_idx <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_idx, ]
test_data <- data[-train_idx, ]

#建立LightGBM回归模型
params <- list(
  objective = "regression",
  metric = "rmse",
  boosting_type = "gbdt",
  learning_rate = 0.08,
  num_leaves = 146,
  feature_fraction = 0.9,
  max_depth = 13,
  min_data_in_leaf = 12,
  bagging_fraction = 0.8,
  bagging_freq = 8,
  max_bin = 455,
  lambda_l1 = 0.1,
  lambda_l2 = 0.2
)

# 转换数据为矩阵格式
train_matrix <- data.matrix(train_data[, c("Tem","Hum","lat","lon","Tem_M","Hum_M")])
test_matrix <- data.matrix(test_data[, c("Tem","Hum","lat","lon","Tem_M","Hum_M")])

# 创建数据集
lgb_train <- lgb.Dataset(data = train_matrix, label = train_data$HW_day)
lgb_test <- lgb.Dataset(data = test_matrix, label = test_data$HW_day)

sink("E:/output.log")
# 训练模型
lgb_model <- lgb.train(
  params = params,
  data = lgb_train,
  nrounds = 3000,
  valids = list(test = lgb_test),  # 用于验证的数据集
  early_stopping_rounds = 10,      # 早停参数
  verbose = 1
)
sink()
# 预测
lgb_pred <- predict(lgb_model, test_matrix)
lgb_pred2 <- predict(lgb_model, train_matrix)

# 计算拟合精度（例如，均方根误差 RMSE）
r_squared <- 1 - sum((test_data$HW_day- lgb_pred)^2) / sum((test_data$HW_day - mean(test_data$HW_day))^2)
print(paste("测试集的R方（R-squared）:", r_squared))

my_partial_plot_with_ice <- partial(lgb_model, pred.var = "Hum", train = train_matrix, quantiles = TRUE, type = "regression", smooth = TRUE, rug = TRUE, plot.engine = "ggplot", ice = TRUE)
# 提取置信区间


data1 <- read.csv("E:/UTCI/Result2_Part2_new/ZoneA/data_A.csv")
data2 <- read.csv("E:/UTCI/Result2_Part2_new/ZoneB/data_B.csv")
data3 <- read.csv("E:/UTCI/Result2_Part2_new/ZoneC/data_C.csv")
data4 <- read.csv("E:/UTCI/Result2_Part2_new/ZoneD/data_D.csv")
data5 <- read.csv("E:/UTCI/Result2_Part2_new/ZoneE/data_E.csv")
data_predict <- rbind(data1,data2,data3,data4,data5)
##反事实情景预测
#data_predict <- read.csv("E:/UTCI/Result2_Part2_new/ZoneE/data_E.csv")
predict_matrix <- data.matrix(data_predict[, c("Tem","Hum","lat","lon","Tem_M","Hum_M")])
lgb_predict <- predict(lgb_model, predict_matrix)
result2 <- cbind(data_predict, lgb_predict)
write.csv(result2,"E:/UTCI/Result2_output_all/Mean_new/nat_model10.csv", row.names = FALSE)

data1 <- read.csv("E:/UTCI/Result2_Part2_true1/ZoneA/data_A2.csv")
data2 <- read.csv("E:/UTCI/Result2_Part2_true1/ZoneB/data_B2.csv")
data3 <- read.csv("E:/UTCI/Result2_Part2_true1/ZoneC/data_C2.csv")
data4 <- read.csv("E:/UTCI/Result2_Part2_true1/ZoneD/data_D2.csv")
data5 <- read.csv("E:/UTCI/Result2_Part2_true1/ZoneE/data_E2.csv")
data_predict2  <- rbind(data1,data2,data3,data4,data5)
##真实值转1度预测
#data_predict2 <- read.csv("E:/UTCI/Result2_Part2_true1/ZoneE/data_E2.csv")
predict_matrix2 <- data.matrix(data_predict2[, c("Tem","Hum","lat","lon","Tem_M","Hum_M")])
lgb_predict2 <- predict(lgb_model, predict_matrix2)
result3 <- cbind(data_predict2, lgb_predict2)
write.csv(result3,"E:/UTCI/Result2_output_all/Mean_new/all_model10.csv", row.names = FALSE)





my_partial_plot_with_ice <- partial(lgb_model, pred.var = "Hum",quantiles = TRUE,probs=1:99/100, train = train_matrix, type = "regression", rug = TRUE, plot.engine = "ggplot")
calculate_confidence_interval <- function(x) {
  n <- length(x)
  mean_val <- mean(x)
  sd_val <- sd(x)
  se_val <- sd_val / sqrt(n)
  t_value <- qt(0.975, df = n - 1)
  lower <- mean_val - t_value * se_val
  upper <- mean_val + t_value * se_val
  return(c(mean = mean_val, sd = sd_val, Lower = lower, Upper = upper))
}
mean_and_sd <- aggregate(yhat ~ Hum, data = my_partial_plot_with_ice, 
                         FUN = calculate_confidence_interval)

data1 <- read.csv("E:/UTCI2_figure/Chapter3/LightGBM2/Hum_d2.csv")

ggplot(data1, aes(x = Hum, y = mean)) +
  geom_line() +  # 绘制曲线
  geom_ribbon(aes(ymin = Lower2, ymax = Upper2), fill = "lightgrey", alpha = 0.5) +
  labs(x = "Hum", y = "Mean Value") +  # 添加轴标签
  theme_minimal()+  # 使用简约主题
  ylim(-0.03, 0.06)



grouped_data <- my_partial_plot_with_ice %>%
  group_by(Hum) %>%
  summarise(
    avg_yhat = mean(yhat),
    max_yhat = max(yhat),
    min_yhat = min(yhat)
  )

# 提取部分依赖图数据
plot_x <- my_partial_plot_with_ice$values[[1]]
plot_i <- my_partial_plot_with_ice$individual
plot_y <- my_partial_plot_with_ice$average[[1]]



# 创建数据框用于绘图
plot_df <- data.frame(x = numeric(), y = numeric())
for (i in 1:length(plot_i)) {
  df_i <- data.frame(x = plot_x, y = plot_i[[i]])
  plot_df <- rbind(plot_df, df_i)
}

# 绘制部分依赖图和个体条件期望
ggplot(plot_df, aes(x = x, y = y)) +
  geom_line(color = 'black', linetype = 'dashed', size = 1.5, alpha = 0.6) +
  labs(title = paste("Partial Dependence Plot of Hum"),
       x = "Hum",
       y = "Partial Dependence") +
  xlim(min(plot_x) - (max(plot_x) - min(plot_x)) * 0.1, max(plot_x) + (max(plot_x) - min(plot_x)) * 0.1) +
  theme_minimal()

p <- ggplot(data.frame(Hum = my_partial_plot2$Hum, yhat = my_partial_plot2$yhat), aes(x = Hum, y = yhat)) +
  geom_line(color = "black", linewidth = 1) +
  labs(title = "Partial Dependence Plot with Rug",
       x = "Hum",
       y = "Partial Dependence") +
  theme_minimal() +
  theme(
    aspect.ratio = 2/3,
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "black", size = 1,fill=NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(size = 10),
    text = element_text(family = "serif"),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

ggsave("E:/UTCI2_figure/partialcurve/pdp_Hum_extreme.png", p, dpi = 600,width = 6,height=4)  





importance <- lgb.importance(model = lgb_model)
print(importance)


##输出拟合值
all_matrix <- data.matrix(data[, c("Tem","Hum","lat","lon","Tem_M","Hum_M")])
lgb_pred_all <- predict(lgb_model, all_matrix)
result <- cbind(data, lgb_pred_all)
# 将结果保存到 CSV 文件
write.csv(result,"E:/UTCI/Result2_output_all/nihe/extreme/all_model1.csv", row.names = FALSE)


