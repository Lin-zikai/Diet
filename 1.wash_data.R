library(data.table)
library(dplyr)

# 数据提取方案 ------

# 1. 读取数据
input_file <- "/data/shared/UKB/all_all.csv"


potential_cols <- c(
  "eid",       #编码
  "p41270",    #ICD编码
  "p31",       #Sex
  "p21001_i0", #BMI
  "p21022",    #Age
  "p20116_i0", #smoke
  "p21000_i0", #Ethnic
  "p131498","p22191_i0","p53_i0"
)

# Efficiently find which of these potential columns actually exist in the file
# (Reads only the header or first few rows to get names)
message("Reading header to find existing columns...")
header_dt <- fread(input_file, nrows = 0)
all_col_names <- names(header_dt) # Get all column names from the header

# Identify explicitly listed potential columns that exist
existing_explicit_cols <- intersect(potential_cols, all_col_names)

# Identify columns matching the p41280_a pattern
p41280_a_cols <- grep("^p41280_a\\d*$", all_col_names, value = TRUE)

# Combine all desired columns
desired_cols <- unique(c(existing_explicit_cols, p41280_a_cols))

missing_potential_cols <- setdiff(potential_cols, desired_cols)
if (length(missing_potential_cols) > 0) {
  message(paste("Note: The following explicitly listed potential columns were not found:", paste(missing_potential_cols, collapse=", ")))
}

if (!"eid" %in% desired_cols) {
  # Try finding eid based on likely patterns if exact match failed
  eid_actual_name <- grep("^eid$|^f.eid$", all_col_names, value = TRUE)[1]
  if(!is.na(eid_actual_name)) {
    message(paste("Found eid column as:", eid_actual_name))
    desired_cols <- c(eid_actual_name, setdiff(desired_cols, "eid")) # Add the actual name and remove the generic "eid" if it was there
  } else {
    stop("Could not find 'eid' column in the input file.")
  }
}

# 查询未找到的编码
# grep("21000",all_col_names, value = TRUE )


# 正式数据提取-----

message(paste("Reading data for", length(desired_cols), "columns..."))
# Read only the existing relevant columns
all <- fread(input_file, select = desired_cols)


# 2. 清洗并拆分 p41270 列：去除中括号和单引号，然后按逗号分割
split_strings <- strsplit(
  gsub("\\[|\\]|'", "", all$p41270),
  ",\\s*"
)

# 3. 定义需要查找的代码范围
#
codes <- c(
  "J440", "J441", "J448", "J449", # COPD
  "J450", "J458", "J459",  # 哮喘
  "J47", # 支气管扩张

  "J40","J410" ,"J411","J42", # 慢性支气管炎
  "J430", "J431", "J432", "J438", "J439", # 肺气肿
  "J841"  # IPF
)

# 4. 为每个代码找到其在 split_strings 中的位置
# 创建一个矩阵，行对应样本，列对应代码
C34_indexes <- sapply(codes, function(code) {
  sapply(split_strings, function(x) {
    pos <- match(code, x)
    if (length(pos) == 0) NA else pos
  })
})
# C34_indexes 的维度为 (样本数) x (代码数)

# 5. 确定需要提取的 p41280_aX 列
max_index <- max(C34_indexes, na.rm = TRUE)  # 找到所有索引中的最大值
col_needed <- paste0("p41280_a", seq_len(max_index))
# 提取这些列并转为矩阵以提高访问速度
p41280_matrix <- as.matrix(all[, col_needed])

# 6. 准备行索引
row_idx <- seq_len(nrow(all))

# 7. 初始化一个列表来存储每个代码对应的日期
C34_dates <- list()

# 8. 对每个代码进行处理
for (i in seq_along(codes)) {
  code <- codes[i]
  index <- C34_indexes[, i]

  # 处理 NA 索引：临时替换为 1，以避免矩阵索引错误
  idx_na <- is.na(index)
  temp_index <- index
  temp_index[idx_na] <- 1  # 任意有效索引

  # 使用矩阵索引获取对应的值
  values <- p41280_matrix[cbind(row_idx, temp_index)]

  # 将原本应为 NA 的位置重新设为 NA
  values[idx_na] <- NA

  # 转换为 Date 类型
  C34_dates[[code]] <- as.Date(values)
}

# 9. 将结果存回原数据表中，每个代码对应一个新列
for (code in codes) {
  all[[code]] <- C34_dates[[code]]
}

# 示例：查看 C340 列中的前几行
# head(na.omit(all$J841))
all<- as.data.frame(all)

lc <- all[,c("eid","p31",codes,"p21001_i0","p21022","p20116_i0","p22191_i0","p53_i0")]


lc$asthma <- apply(lc[, c("J450", "J458", "J459")], 1, function(x) {
  if (all(is.na(x))) {
    return(NA)
  } else {
    return(min(x, na.rm = TRUE))
  }
})


lc$COPD <- apply(lc[, c("J440", "J441", "J448", "J449")], 1, function(x) {
  if (all(is.na(x))) {
    return(NA)
  } else {
    return(min(x, na.rm = TRUE))
  }
})


lc$bronchitis <- apply(lc[, c("J40", "J410", "J411", "J42")], 1, function(x) {
  if (all(is.na(x))) {
    return(NA)
  } else {
    return(min(x, na.rm = TRUE))
  }
})

lc$emphysema <- apply(lc[, c("J430", "J431", "J432", "J438", "J439")], 1, function(x) {
  if (all(is.na(x))) {
    return(NA)
  } else {
    return(min(x, na.rm = TRUE))
  }
})

head(na.omit(lc$asthma))
head(lc,20)



lc <-lc %>% dplyr::rename("Sex" = "p31", "BMI" = "p21001_i0", "Age" = "p21022", "smoke" = "p20116_i0", "telomere" = "p22191_i0", "start_data" = "p53_i0", "Bronchiectasis"="J47","IPF"="J841")
final <- lc[,c("eid","Sex","BMI","Age","smoke","telomere","start_data","asthma","COPD","bronchitis","emphysema","Bronchiectasis","IPF")]


# 仅保留COPD，并保存文件
final <- lc[,c("eid","COPD")]
fwrite(final,"COPD.csv")

