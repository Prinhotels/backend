namespace hs Model.Database


enum status_reg {
  active,
  inactive,
  stored,
  deleted
}

struct TPR {
  1: optional i32 tpr_id = 0,
  2: string tpr_name = "",
  3: i32 tpr_year = 0,
  4: string tpr_description = "",
  5: i32 tpr_hot_id = 0,
  6: string tpr_restriction = "",
  7: optional i32 tpr_min_nights = 0,
  20: status_reg tpr_status_reg,
  21: i32 tpr_updated_by
}

service Season{
  map<i32,i32> get (1:required i32 hot_id, 2:required string dateFrom, 3:required string dateTo);
  list<TPR> getInfo (1:required i32 hot_id, 2:required string dateFrom, 3:required string dateTo);
}
