y <- function(x) {
  # library("haven")
  # dataset <- read_sas("/project/cdc-k-cure/rgst_nhis_jk_add_id_total_stat.sas7bdat")
  # head(dataset)
  # colnames(dataset) #변수들 이름
  # source("functions.R")
  # ## 실행 전 # 아래 '중요!!' 참조
  # dataset <- dataset[!is.na(dataset$total_cost) & (dataset$total_cost != 0), ] # 'total_cost'가 의료비에 해당하는 변수인지 확인할 것.
  # ### 중요!!!
  # # 'dataset$total_cost'는 dataset의 total_cost에 해당하는 열을 불러오는 작업임
  # # 그런데, 각 변수의 정확한 명칭이 생각나지 않아서 대충 적어놓은 것이기 때문에
  # # colnames(dataset)을 통해 해당하는 변수의 이름을 바꿔서 적어야 함
  # # 예로, 만약 dataset의 cancer이 암으로 분류된 의료비라면, cancer <- dataset$cancer로 설정하면 됨
  # # 해당 작업을 아래 모든 변수에서 똑같이 해줄 것!!
  #
  # dataset$SIDO_CD[is.na(dataset$SIDO_CD)] <- "unknown" # sido의 결측값을 unknown으로 대체
  #
  # #------ 기존 5세 단위 나이를 10세 단위로 바꾸고 싶은 경우 실행 -------#
  # age10 <- age <- dataset$age
  # age10[age %in% c("A00_04", "A05_09")] <- "A00_09"
  # age10[age %in% c("A10_15", "A15_19")] <- "A10_19"
  # age10[age %in% c("A20_24",	"A25_29")] <- "A20_29"
  # age10[age %in% c("A30_34",	"A35_39")] <- "A30_39"
  # age10[age %in% c("A40_44",	"A45_49")] <- "A40_49"
  # age10[age %in% c("A50_54",	"A55_59")] <- "A50_59"
  # age10[age %in% c("A60_64",	"A65_69")] <- "A60_69"
  # age10[age %in% c("A70_74",	"A75_79")] <- "A70_79"
  # age10[age %in% c("A80_84",	"A85_plus")] <- "A80_plus"
  # age <- age10
  # dataset$age <- age10
  # #---------------------------------------------------------------------#
  #
  # # p = 0.1 (about 10% sampling)
  #
  # ## 표본추출 함수는 fn_sampling과 fn_sampling_prop가 있음.
  # ## 기본적으로, 대표변수 y, 식별번호 id, 층으로 사용할 변수로 구성된 객체(행렬)을 사용해야 함
  # ## 다른 옵션은 다음과 같음
  # ### p = 0.1 <= 모집단 크기의 대략 10%를 표본추출함
  # ### txtname <= 결과가 저장될 csv파일 이름
  # ## 다음 전체암의 예제를 돌린 후 추후 유방암과 위암을 돌릴 것
  #
  # #######################################
  # ########## 대표변수: 의료비  ##########
  # #######################################
  # ## 전체암
  # y <- log(dataset$total_cost)
  # id <- dataset$id
  # strata <- dataset[c("YEAR", "SIDO_CD", "SEX_TYPE", "cancer", "SEER_GRP1", "age")]
  # txtname <- "allcancers"
  #
  # sampleID_allcancers <- fn_sampling(y, strata, id, p = 0.1, txtname)
  # round(nrow(sampleID_allcancers)/length(y) * 100, 1) # 표본크기 비율
  #
  # ## 유방암
  # dataset_Breast <- dataset[dataset$cancer == "Breast", ] # cancer가 암종에 해당하는 변수 이름인지 확인할 것. 아니면 교체
  # y <- log(dataset_Breast$total_cost)
  # id <- dataset_Breast$id
  # strata <- dataset_Breast[c("YEAR", "SIDO_CD", "SEX_TYPE", "cancer", "SEER_GRP1", "age")]
  # txtname <- "Breast"
  #
  # sampleID_Breast <- fn_sampling(y, strata, id, p = 0.1, txtname, sampleID_allcancers)
  # round(nrow(sampleID_Breast)/length(y) * 100, 1)
  #
  # ## 위암
  # dataset_Stomach <- dataset[dataset$cancer == "Stomach", ]
  # y <- log(dataset_Stomach$total_cost)
  # id <- dataset_Stomach$id
  # strata <- dataset_Stomach[c("YEAR", "SIDO_CD", "SEX_TYPE", "cancer", "SEER_GRP1", "age")]
  # txtname <- "Stomach"
  #
  # sampleID_Stomach <- fn_sampling(y, strata, id, p = 0.1, txtname, sampleID_allcancers)
  # round(nrow(sampleID_Stomach)/length(y) * 100, 1)
  #
  # #######################################
  # ######## 대표변수: 병기 비율 ##########
  # #######################################
  # ## 전체암
  # y <- dataset$SEER_GRP1 # 병기에 관한 변수를 y에 저장
  # id <- dataset$id
  # strata <- dataset[c("YEAR", "SIDO_CD", "SEX_TYPE", "cancer", "age")]  # 병기는 strata에서 제외
  # txtname <- "allcancers"
  #
  # sampleID_stage_allcancers <- fn_sampling_stage(y, strata, id, p = 0.1, txtname)
  # round(nrow(sampleID_stage_allcancers)/length(y) * 100, 1)
  #
  # ## 유방암
  # dataset_Breast <- dataset[dataset$cancer == "Breast", ] # cancer가 암종에 해당하는 변수 이름인지 확인할 것. 아니면 교체
  # y <- dataset_Breast$SEER_GRP1
  # id <- dataset_Breast$id
  # strata <- dataset_Breast[c("YEAR", "SIDO_CD", "SEX_TYPE", "cancer", "age")]
  # txtname <- "Breast"
  #
  # sampleID_stage_Breast <- fn_sampling_stage(y, strata, id, p = 0.1, txtname, sampleID_stage_allcancers)
  # round(nrow(sampleID_stage_Breast)/length(y) * 100, 1)
  #
  # ## 위암
  # dataset_Stomach <- dataset[dataset$cancer == "Stomach", ]
  # y <- dataset_Stomach$SEER_GRP1
  # id <- dataset_Stomach$id
  # strata <- dataset_Stomach[c("YEAR", "SIDO_CD", "SEX_TYPE", "cancer", "age")]
  # txtname <- "Stomach"
  #
  # sampleID_stage_Stomach <- fn_sampling_stage(y, strata, id, p = 0.1, txtname, sampleID_stage_allcancers)
  # round(nrow(sampleID_stage_Stomach)/length(y) * 100, 1)
  #
  #
  # ######################################
  # ######## 변수별 대표성 평가 ##########
  # ######################################
  # ## 본 방법을 통해 여러 층화변수 조합에서 의료비와 병기비율의 대표성 여부를 알 수 있음
  #
  # ## 의료비 사용
  # #전체암
  # pop.DB <- dataset
  # pop.DB$y <- log(dataset$total_cost) # dataset(모집단)에서 '로그의료비'에 해당되는 변수를 y에 새로 저장
  # sample.DB <- sampleID_allcancers
  # var.strata <- c("YEAR", "SIDO_CD", "SEX_TYPE", "cancer", "age") # 원하는 변수조합으로 조절 가능
  # # var.strata <- c("YEAR", "SEX_TYPE", "age") # 다른 예제
  # table_cost <- fn_representative("cost", var.strata, sample.DB, pop.DB)
  # table_cost
  #
  # #유방암
  # pop.DB <- dataset_Breast
  # pop.DB$y <- log(dataset_Breast$total_cost) # dataset(모집단)에서 '로그의료비'에 해당되는 변수를 y에 새로 저장
  # sample.DB <- sampleID_Breast
  # var.strata <- c("YEAR", "SIDO_CD", "SEX_TYPE", "cancer", "SEER_GRP1", "age") # 원하는 변수조합으로 조절 가능.
  # table_cost <- fn_representative("cost", var.strata, sample.DB, pop.DB)
  # table_cost
  #
  # #위암
  # pop.DB <- dataset_Stomach
  # pop.DB$y <- log(dataset_Stomach$total_cost) # dataset(모집단)에서 '로그의료비'에 해당되는 변수를 y에 새로 저장
  # sample.DB <- sampleID_Stomach
  # var.strata <- c("YEAR", "SIDO_CD", "SEX_TYPE", "cancer", "SEER_GRP1", "age") # 원하는 변수조합으로 조절 가능.
  # table_cost <- fn_representative("cost", var.strata, sample.DB, pop.DB)
  # table_cost
  #
  #
  # ## 병기비율
  # #전제암
  # dataset$stage <- dataset$SEER_GRP1 # dataset(모집단)에서 '병기'에 해당되는 변수를 y에 새로 저장
  # var.strata <- c("YEAR", "SIDO_CD", "SEX_TYPE", "cancer", "age") # 층을 뺄수록 속도가 느려짐
  # table_stage <- fn_representative("stage", var.strata, sampleID_stage_allcancers, dataset)
  #
  # #유방암
  # dataset_Breast$stage <- dataset_Breast$SEER_GRP1 # dataset(모집단)에서 '병기'에 해당되는 변수를 y에 새로 저장
  # var.strata <- c("YEAR", "SIDO_CD", "SEX_TYPE", "cancer", "age") # 너무 줄이면 속도가 느려짐
  # table_stage <- fn_representative("stage", var.strata, sampleID_stage_Breast, dataset_Breast)
  #
  # #위암
  # dataset_Stomach$stage <- dataset_Stomach$SEER_GRP1 # dataset(모집단)에서 '병기'에 해당되는 변수를 y에 새로 저장
  # var.strata <- c("YEAR", "SIDO_CD", "SEX_TYPE", "cancer", "age") # 너무 줄이면 속도가 느려짐
  # table_stage <- fn_representative("stage", var.strata, sampleID_stage_Stomach, dataset_Stomach)
  #
  #
  #
  x

}
