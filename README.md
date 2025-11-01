UKB COPD 数据提取
本项目包含一个 R 脚本，用于从 UK Biobank (UKB) 的大型数据集 (all_all.csv) 中提取与慢性阻塞性肺疾病 (COPD) 相关的数据。

脚本使用 data.table 和 dplyr 包，根据 UKB 字段 p42016（COPD 诊断日期）是否存在，来定义 COPD 病例组（1）和健康对照组（0）。

最终输出一个名为 COPD.csv 的文件，包含三列：eid（参与者ID）、COPD_date（诊断日期）和 COPD（病例/对照状态）。
