# 載入必要的套件
library(ggplot2)
library(dplyr)
# 繪製折線圖
plot <- ggplot(total_births_long, aes(x = 年份, y = 出生數, color = 性別, group = 性別)) +
  geom_line(size = 1.2) +  # 繪製折線
  geom_point(size = 3) +  # 添加數據點
  geom_text(
    aes(label = 出生數),
    size = 4,
    vjust = ifelse(total_births_long$年份 == "111年", 1.5, -0.5)  # 111年的標籤下移，其他上移
  ) +
  labs(
    x = "年份",
    y = "出生數量",
    title = "台南市各年份出生數男與出生數女變化"
  ) +
  theme_minimal(base_size = 10) +  # 使用簡潔主題，並縮小基礎字體大小
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # 調整x軸標籤
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  scale_color_manual(values = c("blue", "pink")) +  # 自定義顏色
  coord_cartesian(clip = "on")  # 保證完整呈現圖表

# 顯示圖表
print(plot)

