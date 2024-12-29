# 載入必要的套件
library(ggplot2)  # 用於繪製圖形
library(dplyr)    # 用於數據處理
library(tidyr)    # 用於數據處理

# 範例資料：110年到112年的出生數據
total_births_long <- data.frame(
  年份 = factor(c("110年", "111年", "112年"), levels = c("110年", "111年", "112年")),
  性別 = rep(c("男生出生數", "女生出生數"), each = 3),
  出生數 = c(8000, 7600, 8500, 7800, 7400, 8200)  # 範例的出生數據
)

# 計算總出生數（男生 + 女生）並新增一列資料
total_births_long <- total_births_long %>%
  group_by(年份) %>%
  mutate(總出生數 = sum(出生數)) %>%
  ungroup()

# 使用線性回歸模型進行預測
# 預測女生出生數
model_female <- lm(出生數 ~ as.numeric(年份), data = total_births_long %>% filter(性別 == "女生出生數"))
# 預測男生出生數
model_male <- lm(出生數 ~ as.numeric(年份), data = total_births_long %>% filter(性別 == "男生出生數"))
# 預測總出生數
model_total <- lm(總出生數 ~ as.numeric(年份), data = total_births_long)

# 建立未來三年的年份資料
future_years <- data.frame(年份 = factor(c("113年", "114年", "115年"), levels = c("113年", "114年", "115年")))

# 對未來三年進行預測
pred_female <- predict(model_female, newdata = future_years)
pred_male <- predict(model_male, newdata = future_years)
pred_total <- predict(model_total, newdata = future_years)

# 將預測結果與原始資料合併
predictions <- data.frame(
  年份 = rep(future_years$年份, each = 2),  # 重複未來三年的年份，這裡對應性別的數據為兩個
  性別 = rep(c("男生出生數", "女生出生數"), times = 3),  # 為每個性別創建對應的預測資料
  出生數 = c(pred_male, pred_female),  # 將預測結果放入出生數列
  類型 = "預測"  # 標註資料類型為預測
)

# 在總出生數預測資料中，為每個年份建立 "總出生數" 資料
total_predictions <- data.frame(
  年份 = rep(future_years$年份, each = 1),  # 每年對應三個性別
  性別 = "總出生數",  # 將總出生數的性別標註為 "總出生數"
  出生數 = pred_total,  # 將總出生數預測結果放入
  類型 = "預測"
)

# 合併預測資料
all_data <- bind_rows(predictions, total_predictions)

# 繪製圖形，只顯示113年到115年的資料
ggplot(all_data, aes(x = 年份, y = 出生數, fill = 性別)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # 繪製長條圖，將男女出生數分開顯示
  labs(x = "年份", y = "出生數量", title = "台南市未來三年出生數男與出生數女變化及總出生數預測") +  # 圖表標題與軸標籤
  scale_fill_manual(values = c("男生出生數" = "blue", "女生出生數" = "pink", "總出生數" = "gray")) +  # 設定顏色
  theme_minimal(base_size = 12) +  # 設定主題為簡約風格
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # X軸標籤旋轉
    legend.title = element_text(size = 10),  # 圖例標題字型大小
    legend.text = element_text(size = 8)  # 圖例內容字型大小
  )


