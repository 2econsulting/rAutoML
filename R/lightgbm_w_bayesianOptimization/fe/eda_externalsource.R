# title : eda_externalsource
# author : jacob 


eda_externalsource <- function(data, vi){
  
  data[,NEW_EXT_SOURCE_nasum := apply(.SD, 1, function(x) sum(is.na(x))), .SDcols=vi]
  data[,NEW_EXT_SOURCE_rowsum := sum(EXT_SOURCE_1,EXT_SOURCE_2,EXT_SOURCE_3,na.rm=T)]
  data[,NEW_EXT_SOURCE_weight1 := (EXT_SOURCE_1*2+EXT_SOURCE_2*3+EXT_SOURCE_3*4)/9]
  data[,NEW_EXT_SOURCE_weight2 := (EXT_SOURCE_1*3+EXT_SOURCE_2*4+EXT_SOURCE_3*2)/9]
  data[,NEW_EXT_SOURCE_weight3 := (EXT_SOURCE_1*4+EXT_SOURCE_2*2+EXT_SOURCE_3*3)/9]
  
  return(data)
}






