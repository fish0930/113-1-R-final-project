# 載入必要套件
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  message("tidyverse 尚未安裝，正在安裝中...")
  install.packages("tidyverse")
}
library(tidyverse)

# 匯入 CSV 檔案
data_110 <- read_csv("110.csv")
data_111 <- read_csv("111.csv")
data_112 <- read_csv("112.csv")

# 計算每年總出生數、男生出生數和女生出生數
total_110 <- data_110 %>%
  summarise(總出生數 = sum(出生數計),
            男生出生數 = sum(出生數男),
            女生出生數 = sum(出生數女)) %>%
  mutate(年份 = "110年")

total_111 <- data_111 %>%
  summarise(總出生數 = sum(出生數計),
            男生出生數 = sum(出生數男),
            女生出生數 = sum(出生數女)) %>%
  mutate(年份 = "111年")

total_112 <- data_112 %>%
  summarise(總出生數 = sum(出生數計),
            男生出生數 = sum(出生數男),
            女生出生數 = sum(出生數女)) %>%
  mutate(年份 = "112年")

# 合併三個年份的資料
total_births <- bind_rows(total_110, total_111, total_112)

# 轉換資料框格式，將出生數分為男生和女生
total_births_long <- total_births %>%
  pivot_longer(cols = c(男生出生數, 女生出生數), 
               names_to = "性別", values_to = "出生數")

# 繪製堆疊長條圖
ggplot(total_births_long, aes(x = 年份, y = 出生數, fill = 性別)) + 
  geom_bar(stat = "identity", position = "stack") +  # 堆疊顯示
  labs(x = "年份", y = "總出生數", title = "台南市每年出生數按性別分組") +  # 標題與軸標籤
  scale_fill_manual(values = c("男生出生數" = "blue", "女生出生數" = "pink")) +  # 設定顏色
  theme_minimal() +  # 使用簡潔的圖形風格
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # 調整x軸標籤角度

