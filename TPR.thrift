namespace hs Database.Definition

enum status_reg {
  active,
  inactive,
  stored,
  deleted
}

struct TPR {
  1: optional i32 tpr_id = 0,
  2: i32 tpr_name = 0,
  3: i32 tpr_year = 0,
  4: i32 tpr_description = 0,
  5: i32 tpr_hot_id = 0,
  6: i32 tpr_restriction = 0,
  7: optional i32 tpr_min_nights = 0,
  8: string tpr_data_5 = "",
  9: string tpr_data_6 = "",
  10: string tpr_data_7 = "",
  11: string tpr_data_8 = "",
  12: string tpr_data_9 = "",
  13: string tpr_data_10 = "",
  14: string tpr_data_11 = "",
  15: string tpr_data_12 = "",
  16: string tpr_data_1 = "",
  17: string tpr_data_2 = "",
  18: string tpr_data_3 = "",
  19: string tpr_data_4 = "",
  20: status_reg tpr_status_reg,
  21: i32 tpr_updated_by,
  22: optional string tpr_updated = "",
  23: optional string tpr_created = ""
}