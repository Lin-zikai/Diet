library(data.table)
library(dplyr)

# 数据提取方案 ------

# 1. 读取数据
input_file <- "/data/shared/UKB/all_all.csv"


potential_cols <- c(
  "eid",       #编码
"p42016"
)
# Efficiently find which of these potential columns actually exist in the file
# (Reads only the header or first few rows to get names)
message("Reading header to find existing columns...")
header_dt <- fread(input_file, nrows = 0)

all <- fread(input_file, select = potential_cols)
all <-all %>% dplyr::rename(  "COPD_date" = "p42016" )
all$COPD <- ifelse(is.na(all$COPD_date),0,1)
fwrite(all,"COPD.csv")

# 结果包括三列eid，COPD_date COPD诊断时间，COPD 0为健康对照 1为COPD组
