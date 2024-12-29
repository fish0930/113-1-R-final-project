# 載入必要套件
library(ggplot2)
library(dplyr)
library(tidyr)

# 假設資料已經存在，這裡會使用一些範例資料
# data_110, data_111, data_112 是您的原始資料集

# 合併三個資料集並添加年份資訊
age_births <- bind_rows(
  data_110 %>% mutate(年份 = "110年"),
  data_111 %>% mutate(年份 = "111年"),
  data_112 %>% mutate(年份 = "112年")
)

# 計算總出生數
total_births <- age_births %>%
  group_by(年份) %>%
  summarise(
    出生數男 = sum(出生數男, na.rm = TRUE),
    出生數女 = sum(出生數女, na.rm = TRUE),
    總出生數 = sum(出生數男, na.rm = TRUE) + sum(出生數女, na.rm = TRUE)
  )

# 準備長條圖資料
bar_data <- total_births %>%
  pivot_longer(cols = c(出生數男, 出生數女), names_to = "性別", values_to = "數量")

# 準備折線圖資料（僅包含總出生數）
line_data <- total_births %>%
  select(年份, 總出生數) %>%
  mutate(年齡層 = "總出生數", 數量 = 總出生數) %>%
  select(-總出生數)

# 顏色設置（折線圖顏色為黑色，因為只有總出生數）
line_colors <- c("總出生數" = "black")

# 繪製圖表
ggplot() +
  # 長條圖部分
  geom_bar(
    data = bar_data,
    aes(x = 年份, y = 數量, fill = 性別),
    stat = "identity",
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  # 折線圖部分（只顯示總出生數的折線圖）
  geom_line(
    data = line_data,
    aes(x = 年份, y = 數量, group = 年齡層, color = 年齡層),
    size = 1.2,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = line_data,
    aes(x = 年份, y = 數量, color = 年齡層),
    size = 2,
    inherit.aes = FALSE
  ) +
  # 標籤與主題
  labs(
    x = "年份",
    y = "數量",
    title = "台南市各年份出生數與總出生數變化（長條圖與折線圖）",
    fill = "性別",
    color = "年齡層"
  ) +
  scale_fill_manual(values = c("pink","blue" )) +  # 長條圖顏色
  scale_color_manual(values = line_colors) +  # 折線圖顏色
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.position = "bottom"
  ) +
  theme(legend.key.size = unit(0.6, "cm"))  # 調整圖例大小


