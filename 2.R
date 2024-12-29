# 載入必要的套件
library(ggplot2)
library(dplyr)

# 轉換數據格式，保留年份和年齡層的數據
total_mother_long <- total_mother %>%
  pivot_longer(cols = starts_with("生母年齡"),
               names_to = "生母年齡層",
               values_to = "生母數量")

# 繪製折線圖
ggplot(total_mother_long, aes(x = 年份, y = 生母數量, color = 生母年齡層, group = 生母年齡層)) +
  geom_line(size = 1.2) +  # 折線
  geom_point(size = 3) +  # 數據點
  labs(
    x = "年份",
    y = "生母數量",
    title = "台南市各年齡層生母數量變化"
  ) +
  theme_minimal() +  # 簡潔的主題
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # 調整 x 軸標籤
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  scale_color_brewer(palette = "Set3")  # 設定顏色

