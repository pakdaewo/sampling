fn_sample <- function(x, N, n) {
  Ni <- length(x)
  ni <- min(Ni, ceiling(n * Ni/N))
  ind <- rep(0, Ni)
  ind[sample(Ni, ni)] <- 1

  return(ind)
}

# fn_sample <- function(x, Ni, ni) {
#   ind <- rep(0, Ni)
#   ind[sample(Ni, ni)] <- 1
#
#   return(ind)
# }


fn_optim.nh <- function(x, optim_n, N) {

  Ni <- length(x)
  optim.ni <- min(Ni, ceiling(optim_n * Ni/N))

  return(optim.ni)
}

fn_weight <- function(sampled) {
  Ni <- length(sampled)
  ni <- sum(sampled)
  w <- Ni/ni

  return(w)
}

fn_sd <- function(y, sampled, pop = TRUE) {
  if (length(y) == sum(sampled)) {
    res <- NA
  } else {
    if(pop == TRUE) res <- sqrt(sum((y - mean(y))^2)/length(y))
    if(pop == FALSE) res <- ifelse(length(y) > 1, sd(y), NA)
  }
  return(res)
}

fn_represent_each <- function(mean.y, mean.sy, sd, Ni, ni, ...) {

  za <- qnorm(0.975)
  Lower_CI <- mean.sy - za * sqrt((Ni - ni)/(Ni*ni)) * sd
  Upper_CI <- mean.sy + za * sqrt((Ni - ni)/(Ni*ni)) * sd
  represent_ind <- (Lower_CI <= mean.y) & (Upper_CI >= mean.y)
  represent_ind[is.na(represent_ind)] <- 1
  return(represent_ind)

}

fn_represent2 <- function(mean.y, mean.sy, sd, Ni, ni, wp, ...) {

  za <- qnorm(0.975)
  mean.y_st <- sum(wp * mean.y)
  mean.sy_st <- sum(wp * mean.sy)
  var.y_st <- sum(wp^2 * (Ni - ni)/(Ni*ni) * sd^2)
  sd.y_st <- sqrt(var.y_st)

  Lower_CI <- mean.sy_st - za * sd.y_st
  Upper_CI <- mean.sy_st + za * sd.y_st
  represent_ind <- (Lower_CI <= mean.y_st) & (Upper_CI >= mean.y_st)
  represent_ind[is.na(represent_ind)] <- 1

  return(represent_ind)

}

fn_check <- function(main_data, xnames, ...) {

  res_data <- main_data %>%
    summarise(Ni = length(sampled), ni = sum(sampled), mean.y = mean(y), mean.sy = mean(y[sampled == 1]), sd.y = fn_sd(y, sampled))

  final_check <- "SUCCESS"

  # one variable
  for (i in 1:length(xnames)) {

    temp_data <- res_data %>% group_by(!!!rlang::syms(xnames[i]))
    temp_res <- temp_data %>% mutate(wp = Ni/sum(Ni)) %>% summarise(represent = fn_represent2(mean.y, mean.sy, sd.y, Ni, ni, wp))
    check_temp <- all(temp_res$represent == 1)

    if (check_temp == FALSE) {
      final_check <- "FAIL"
      break
    }

  }

  # two variables
  list_combn1 <- combn(xnames, 2, simplify = FALSE)
  list_combn2 <- combn(xnames, 3, simplify = FALSE)
  # list_combn <- c(list_combn1[-c(4, 8, 11, 13, 15)], list_combn2[-c(3, 6, 8, 12, 19)]) # check
  list_combn <- list_combn1[-c(4, 8, 11, 13, 15)]
  for (j in 1:length(list_combn)) {

    temp_data <- res_data %>% group_by(!!!rlang::syms(list_combn[[j]]))
    temp_res <- temp_data %>% mutate(wp = Ni/sum(Ni)) %>% summarise(represent = fn_represent2(mean.y, mean.sy, sd.y, Ni, ni, wp))
    check_temp <- mean(temp_res$represent == 1)

    if (check_temp < 0.95) {
      final_check <- "FAIL"
      break
    }

  }

  return(final_check)

}

fn_sampling_guaranteed <- function(y, strata, id, p = 0.1, txtname, list_sampledID = NULL, ...) {

  if(!is.null(list_sampledID)) list_sampledID <- as.data.frame(list_sampledID)

  y <- as.numeric(y)
  id <- as.numeric(id)
  strata <- as.matrix(strata)
  xnames <- colnames(strata)
  nc <- length(xnames)
  idystrata <- data.frame(id, y, strata)
  N <- length(y)
  optim_n <- ceiling(N * p)

  res_check <- "FAIL"
  p0 <- 0.2

  main_data0 <- idystrata %>% group_by(!!!rlang::syms(xnames))

  while (res_check == "FAIL") {

    tryi <- 0

    while (res_check == "FAIL" & tryi < 20) {

      n <- ceiling(N * p0)
      main_data <- main_data0 %>% mutate(sampled = fn_sample(y, N, n))

      # tall <- as.data.frame(main_data) %>% summarise(Ni = length(sampled), ni = sum(sampled), mean.y = mean(y), sd.y = sqrt(sum((y - mean(y))^2)/length(y)), mean.sy = mean(y[sampled == 1]))
      # fn_represent_each(tall$mean.y, tall$sd.y, tall$mean.sy, tall$Ni, tall$ni)
      #

      # res_data <- main_data %>%
      #   summarise(Ni = length(sampled), ni = sum(sampled), mean.y = mean(y), mean.sy = mean(y[sampled == 1]), sd.y = fn_sd(y, sampled))
      # check1_data <- res_data %>% mutate(represent = fn_represent_each(mean.y, mean.sy, sd.y, Ni, ni))
      #
      # notok <- check1_data[check1_data$represent == 0,]
      #
      # if (nrow(notok) != 0) {
      #   for (r in 1:nrow(notok)) {
      #
      #     ind <- TRUE
      #     for (k in 1:nc) {
      #       ind <- ind & (main_data[,k+2] == as.character(notok[r,k]))
      #     }
      #
      #     represent <- 0
      #     while (represent == 0) {
      #       temp_data <- main_data[ind, ] %>% mutate(sampled = fn_sample(y, N, n))
      #       temp_res <- temp_data %>%
      #         summarise(Ni = length(sampled), ni = sum(sampled), mean.y = mean(y), mean.sy = mean(y[sampled == 1]),
      #                   sd.y = fn_sd(y, sampled)) %>% mutate(represent = fn_represent_each(mean.y, mean.sy, sd.y, Ni, ni))
      #       represent <- temp_res$represent
      #     }
      #
      #     main_data[ind, ] <- temp_data
      #
      #     readline(prompt = "Enter term:")
      #   }
      # }

      res_check <- fn_check(main_data, xnames)

      tryi <- tryi + 1
      print(tryi)
    }

    if (res_check == "FAIL") {
      p0 <- p0 + 0.1
    }

    print(p0)

  }

  main_data2 <- main_data  %>% mutate(Ni = length(sampled), optim.nh = fn_optim.nh(y, optim_n, N), diff =  ni - optim.nh, w = fn_weight(sampled))
  temp_data <- main_data3 <- main_data2

  while (mean(main_data3$sampled) > 0.12) {
    ## optim.nh보다 더 크고 w가 더 1보다 큰 것들 중에 제일 작은 것

    candidate.data <- main_data3[which((main_data3$sampled == 1) & (main_data3$diff > 0) & (main_data3$Ni > 1)),]
    candidate.data2 <- candidate.data[candidate.data$diff == max(candidate.data$diff), ]
    candidate.id <- candidate.data2[candidate.data2$w == min(candidate.data2$w), ]$id

    while(length(candidate.id) > 0) {
      rm.id <- as.numeric(sample(as.character(candidate.id), 1))
      temp_data <- main_data3
      temp_data[temp_data$id == rm.id, ]$sampled <- 0
      temp_data <- temp_data %>% mutate(ni = sum(sampled), diff =  ni - optim.nh, w = fn_weight(sampled))
      temp_check <- fn_check(temp_data, xnames)

      if (temp_check == "SUCCESS") {
        main_data3 <- temp_data
        cat(paste("remove: ", "id=", temp_data[rm.id,1], "/", "YEAR=", temp_data[rm.id,3], "/",
                  "SIDO_CD=", temp_data[rm.id,4], "/","SEX_TYPE=", temp_data[rm.id,5], "/",
                  "cancer=", temp_data[rm.id,6], "/","stage=", temp_data[rm.id,7], "/",
                  "age=", temp_data[rm.id,8], "/","Nh=", temp_data[rm.id,10],"/",
                  "nh=", temp_data[rm.id,14],"optim_nh=", temp_data[rm.id,11],
                  "/","w=", round(temp_data[rm.id,13], 2), "\n",sep = ""))
        break
      }

      candidate.id <- candidate.id[candidate.id != rm.id]

    }

    if (length(candidate.id) == 0) main_data0[candidate.id, ]$sampled <- 2

  }


}
