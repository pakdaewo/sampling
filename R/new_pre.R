fn_summary <- function(x) {
  c(Nh = length(x), meanh = mean(x), SDh = sqrt(sum((x - mean(x))^2)/length(x)))
}


fn_summary_stage <- function(x) {
  tx <- table(x)
  probs <- as.numeric(prop.table(tx))
  c(Nh = length(x), freq.pop = tx, prob.pop = probs)
}


fn_represent <- function(mean.sy, mean.y, sd.y, N, n, za, ...) {

  if(N == n) {
    representi <- "ALL SAMPLED"
  } else {
    if (n >= 1) {
      Lower_CI <- mean.sy - za * sd.y * sqrt((N - n)/(N - 1)/n)
      Upper_CI <- mean.sy + za * sd.y *sqrt((N - n)/(N - 1)/n)
      represent_ind <- (Lower_CI <= mean.y) & (Upper_CI >= mean.y)
      representi <- ifelse(represent_ind == TRUE, "YES", "NO")
    }
  }

  return(representi)

}

findVectors <- function(groups, size) {
  if (groups == 1) {
    mat = size
  }
  else {
    mat <- matrix(rep(0, groups - 1), nrow = 1)
    for (i in 1:size) {
      mat <- rbind(mat, findVectors(groups - 1, i))
    }
    mat <- cbind(mat, size - rowSums(mat))
  }
  invisible(mat)
}

fn_multinomial_test <- function(obs, prob, ...) {
  epsilon <- sqrt(.Machine$double.eps)
  size <- sum(obs)
  pObs <- dmultinom(x = obs, prob = prob)
  eventMat <- findVectors(4, size)
  ex_ind <- apply(eventMat, 1, function(x, prob) sum(x[prob == 0]) > 0, prob = prob)
  eventMat <- eventMat[!ex_ind, ]
  if(is.null(nrow(eventMat))) eventMat <- matrix(eventMat, nrow = 1)
  eventProb <- apply(eventMat, 1, function(x) dmultinom(x, prob = prob))
  p.value <- sum(eventProb[eventProb <= pObs + epsilon])
  return(p.value)
}

dist_p <- function(p, p0) {
  p[is.na(p)] <- 0
  1 - sum(p * p0)/sqrt(sum(p^2) * sum(p0^2))
}


fn_sampling <- function(y, strata, id, p = 0.1, txtname, sampleID_allcancers = NULL, ...) {

  if(!is.null(sampleID_allcancers)) sampleID_allcancers <- as.data.frame(sampleID_allcancers)

  y <- as.numeric(y)
  id <- as.numeric(id)
  strata <- as.matrix(strata)
  nc <- ncol(strata)
  xnames <- colnames(strata)
  all <- expand.grid(apply(strata, 2, function(x) sort(unique(x))))
  nall <- nrow(all)
  list_summary <- aggregate(y, by = apply(strata, 2, function(x) x, simplify = FALSE), FUN = fn_summary)

  # calculating a total size of n
  Nh <- as.matrix(list_summary$x[,1])
  meanh <- as.matrix(list_summary$x[,2])
  SDh <- as.matrix(list_summary$x[,3])

  N <- sum(Nh)
  za <- qnorm(0.975)
  mean.toty <- mean(y)

  #### Result 1
  e_est <- seq(0.01, 0.1, by = 0.01)
  total_ns <- c()
  for (j in 1:length(e_est)) {
    e <- e_est[j]
    num <- (za^2/N^2) * sum(Nh * SDh/(mean.toty^2/sum(Nh * SDh)))
    den <- e + (za^2/N^2) * sum(Nh * SDh^2/(mean.toty^2))
    total_ns[j] <- num/den
  }

  ### 10% sampling
  # Function for checking the representative
  N <- length(y)
  n <- N * p #10% sampling (change it to any proportion for the purpose)
  nh <- pmin(Nh, ceiling(n * sqrt(Nh)/sum(sqrt(Nh)))) # 제곱근비례배분

  table_sampleID <- NULL
  table_summary <- NULL

  for (i in 1:nrow(list_summary)) {

    ind <- TRUE
    for (k in 1:nc) {
      ind <- ind & (strata[,k] == list_summary[i, xnames[k]])
    }

    Ni <- list_summary[i, ]$x[,1]
    mean.y <- list_summary[i, ]$x[,2]
    sd.y <- list_summary[i, ]$x[,3]
    ni <- as.numeric(nh[i])

    IDi <- id[ind]
    yi <- y[ind]

    if(!is.null(sampleID_allcancers)) {
      aind <- TRUE
      for (k in 1:nc) {
        aind <- aind & (sampleID_allcancers[xnames[k]] == list_summary[i, xnames[k]])
      }
      aIDi <- sampleID_allcancers$id[aind]
      length_aID <- length(aIDi)
    } else {
      aIDi <- NULL
      length_aID <- 0
    }

    final_represent <- "NO"
    new_ni <- ni
    iter <- 0

    while(final_represent == "NO") {

      representi <- "NO"
      cn <- 0
      while((representi %in% c("NO")) & cn < 50) {

        if(new_ni > length_aID) {
          addn <- new_ni - length_aID
          sampled_IDs <- sample(x = c(as.character(IDi[!(IDi %in% aIDi)])), size = addn)
          sampled_IDs <- c(aIDi, sampled_IDs)
        } else {
          rmvn <- length_aID - new_ni
          sampled_IDs <- aIDi
          sampled_IDs <- sampled_IDs[!(sampled_IDs %in% sample(x = as.character(aIDi), rmvn))]
        }

        sampled_y <- yi[IDi %in% sampled_IDs]
        mean.sy <- mean(sampled_y)

        if(length(sampled_IDs) > 1) {
          sd.sy <- sd(sampled_y)
          min.sy <- min(sampled_y)
          max.sy <- max(sampled_y)
          quantiles.sy <- quantile(sampled_y, c(0.25, 0.75))
        } else {
          sd.sy <- NA
          min.sy <- NA
          max.sy <- NA
          quantiles.sy <- c(NA, NA)
        }
        representi <- fn_represent(mean.sy, mean.y, sd.y, Ni, new_ni, za)
        summ_sam <- c(mean.sy, sd.sy, min.sy, max.sy, quantiles.sy, representi)
        cn <- cn + 1
      }

      if(cn == 50) {
        new_ni <- new_ni + 1
      } else {
        final_represent <- "YES"
        break
      }

      iter <- iter + 1

    }

    table_summaryi <- c(as.matrix(list_summary[i, ]), summ_sam,  ni = ni, new_ni = new_ni, cn = cn)
    table_summary <- rbind(table_summary, table_summaryi)

    if (ni == 1) {
      table_sampleIDi <- c(as.matrix(list_summary[i, ]), y = sampled_y, id = sampled_IDs)
    } else {
      table_sampleIDi <- cbind(as.matrix(list_summary[i, ])[rep(1, new_ni), ], y = sampled_y, id = sampled_IDs)
    }

    table_sampleID <- rbind(table_sampleID, table_sampleIDi)

    if(i %% 100 == 0) cat(paste(round(i/nrow(list_summary) * 100), "% is done\n", sep = ""))

  }

  colnames(table_summary) <- c(xnames, "Nh", "mean.y", "sd.y",
                               "mean.sy", "sd.sy", "min.sy", "max.sy", "q1.sy", "q3.sy",
                               "represent","n", "new_n", "cn")
  rownames(table_summary) <- NULL
  table_summary <- as.data.frame(table_summary)
  colnames(table_sampleID) <- c(xnames, "Nh", "mean.y", "sd.y", "y", "id")
  rownames(table_sampleID) <- NULL
  table_sampleID <- as.data.frame(table_sampleID)
  filenames <- paste(c("totalns_", "summary_", "sampleID_"), txtname, ".csv", sep = "")
  write.csv(total_ns, filenames[1])
  cat(paste("'", filenames[1], "' is generated.\n", sep = ""))
  write.csv(table_summary, filenames[2])
  cat(paste("'", filenames[2], "' is generated.\n", sep = ""))
  write.csv(table_sampleID, filenames[3])
  cat(paste("'", filenames[3], "' is generated.\n", sep = ""))

  cat(paste("two files are generated in '", getwd(), "'\n", sep = ""))

  res <- table_sampleID
  return(res)

}

fn_sampling_stage <- function(y, strata, id, p = 0.1, txtname, sampleID_allcancers = NULL, ...) {

  if(!is.null(sampleID_allcancers)) sampleID_allcancers <- as.data.frame(sampleID_allcancers)

  stage <- factor(y, levels = sort(unique(y)))
  id <- as.numeric(id)
  strata <- as.matrix(strata)
  nc <- ncol(strata)
  xnames <- colnames(strata)
  all <- expand.grid(apply(strata, 2, function(x) sort(unique(x))))
  nall <- nrow(all)
  list_summary <- aggregate(stage, by = apply(strata, 2, function(x) x, simplify = FALSE), FUN = fn_summary_stage)

  # calculating a total size of n
  Nh <- as.matrix(list_summary$x[,1])
  cath <- as.matrix(list_summary$x[,6:9])
  N <- sum(Nh)

  ### 10% sampling
  # Function for checking the representative
  N <- length(id)
  n <- N * p #10% sampling (change it to any proportion for the purpose)
  nh <- pmin(Nh, ceiling(n * sqrt(Nh)/sum(sqrt(Nh)))) # 제곱근비례배분
  table_sampleID <- NULL
  table_summary <- NULL
  for (i in 1:nrow(list_summary)) {

    ind <- TRUE
    for (k in 1:nc) {
      ind <- ind & (strata[,k] == list_summary[i, xnames[k]])
    }

    Ni <- list_summary[i, ]$x[,1]
    probi <- list_summary[i, ]$x[,6:9]
    ni <- nh[i]

    IDi <- id[ind]
    stagei <- stage[ind]
    table_stagei <- table(stagei)

    if(!is.null(sampleID_allcancers)) {
      aind <- TRUE
      for (k in 1:nc) {
        aind <- aind & (sampleID_allcancers[xnames[k]] == list_summary[i, xnames[k]])
      }
      aIDi <- sampleID_allcancers$id[aind]
      length_aID <- length(aIDi)
    } else {
      aIDi <- NULL
      length_aID <- 0
    }

    final_represent <- "NO"
    new_ni <- max(ni, length(unique(stagei)))
    iter <- 0

    while(final_represent == "NO") {

      pMat <- findVectors(4, new_ni)/new_ni
      onestage <- unique(stagei)
      pMat <- pMat[!apply(pMat, 1, function(x, ...) any(x[!(c(1, 2, 7, 9) %in% onestage)] > 0)),]

      if (is.null(nrow(pMat))) {
        dp_limit <- 1
      } else {
        dp_limit <- quantile(apply(pMat, 1, dist_p, p0 = probi), 0.05) + 0.000000001
      }

      representi <- "NO"
      cn <- 0
      gathering <- FALSE
      notover <- FALSE
      dp <- 1

      pexist <- c(1, 2, 7, 9)[probi > 0]

      while(!((representi %in% c("YES")) & gathering == TRUE & dp < dp_limit & notover == TRUE) & cn < 50) {

        sampledIDs0 <- sampledIDs1 <- NULL
        stage_aIDi <- stagei[IDi %in% aIDi]
        ID_aIDi <- IDi[IDi %in% aIDi]

        if(new_ni > length_aID) {
          addn <- new_ni - length_aID
          for (j in pexist) {
            already_ind <- stagei[IDi %in% aIDi] == j
            if(length(already_ind) == 0) {
              addj <- sample(as.character(IDi[stagei[!(IDi %in% aIDi)] == j]), 1)
            } else {
              addj <- NULL
            }
            sampledIDs0 <- c(sampledIDs0, ID_aIDi[stage_aIDi == j], addj)
          }
          addn2 <- new_ni - length(sampledIDs0)
          sampled_IDs_add2 <- sample(x = c(as.character(IDi[!(IDi %in% sampledIDs0)])), size = addn2)
          sampled_IDs <- c(sampledIDs0, sampled_IDs_add2)
        } else {
          stage_aIDi <- stagei[IDi %in% aIDi]
          ID_aIDi <- IDi[IDi %in% aIDi]

          for (j in pexist) {
            addj <- sample(as.character(ID_aIDi[stage_aIDi == j]), 1)
            sampledIDs1 <- c(sampledIDs1, addj)
          }

          diffn2 <- new_ni - length(sampledIDs1)
          sampled_IDs_diff2 <- sample(x = c(as.character(aIDi[!(aIDi %in% sampledIDs1)])), size = diffn2)
          sampled_IDs <- c(sampledIDs1, sampled_IDs_diff2)
        }

        sampled_stagei <- stagei[IDi %in% sampled_IDs]
        onestage <- unique(stagei)
        if (ni > length(onestage)) {
          gathering <- all(onestage %in% unique(sampled_stagei))
        } else {
          gathering <- TRUE
        }

        sampled_obsi <- table(sampled_stagei)
        sampled_probi <- prop.table(sampled_obsi)
        notover <- all(sampled_obsi <= table_stagei)
        pvalue <- fn_multinomial_test(sampled_obsi, probi)
        representi <- ifelse(pvalue >= 0.05, "YES", "NO")
        dp <- dist_p(sampled_probi, probi)
        cn <- cn + 1

      }

      if(cn == 50) {
        new_ni <- new_ni + 1
      } else {
        final_represent <- "YES"
        break
      }

      iter <- iter + 1

    }

    if(Ni == new_ni) representi <- "ALL SAMPLED"


    table_summaryi <- c(as.matrix(list_summary[i, ]), c(sampled_obsi, sampled_probi), dp = round(dp, 2), ni = ni, new_n = new_ni, represent = representi, cn = cn)
    table_summary <- rbind(table_summary, table_summaryi)

    table_sampleIDi <- as.matrix(list_summary[i, ])

    if (ni == 1) {
      table_sampleIDi <- c(as.matrix(list_summary[i, ]), stage = sampled_stagei, id = sampled_IDs)
    } else {
      table_sampleIDi <- cbind(as.matrix(list_summary[i, ])[rep(1, new_ni), ], stage = sampled_stagei, id = sampled_IDs)
    }

    table_sampleID <- rbind(table_sampleID, table_sampleIDi)
    if(i %% 100 == 0) cat(paste(round(i/nrow(list_summary) * 100), "% is done\n", sep = ""))

  }

  colnames(table_summary) <- c(xnames, "Nh",  "freq1.pop", "freq2.pop", "freq7.pop", "freq9.pop",
                               "prob1.pop", "prob2.pop", "prob7.pop", "prob9.pop",
                               "freq1.sam", "freq2.sam", "freq7.sam", "freq9.sam",
                               "prob1.sam", "prob2.sam", "prob7.sam", "prob9.sam", "dp", "initial_ni", "new_n", "represent", "cn")
  rownames(table_summary) <- NULL
  table_summary <- as.data.frame(table_summary)
  colnames(table_sampleID) <- c(xnames, "Nh", "freq1.pop", "freq2.pop", "freq7.pop", "freq9.pop",
                                "prob1.pop", "prob2.pop", "prob7.pop", "prob9.pop", "stage", "id")
  rownames(table_sampleID) <- NULL
  table_sampleID <- as.data.frame(table_sampleID)
  filenames <- paste(c("summary_stage_", "sampleID_stage_"), txtname, ".csv", sep = "")
  write.csv(table_summary, filenames[1])
  cat(paste("'", filenames[1], "' is generated.\n", sep = ""))
  write.csv(table_sampleID, filenames[2])
  cat(paste("'", filenames[2], "' is generated.\n", sep = ""))

  cat(paste("two files are generated in '", getwd(), "'\n", sep = ""))

  res <- table_sampleID

  return(res)

}

fn_info_continuous <- function(x) {
  c(n = length(x), mean = mean(x), sd = sqrt(sum((x - mean(x))^2)/length(x)))
}

fn_representative <- function(target, var.strata, sample.DB, pop.DB, ...) {

  za <-   za <- qnorm(0.975)
  sample.DB <- as.data.frame(sample.DB)
  pop.DB <- as.data.frame(pop.DB)

  if (target == "cost") {
    y_sam <- as.numeric(sample.DB[, "y"])
    y_pop <- as.numeric(pop.DB[, "y"])

    list_summary_pop <- aggregate(y_pop, by = apply(pop.DB[, var.strata], 2, function(x) x, simplify = FALSE), FUN = fn_info_continuous)
    Nh <- list_summary_pop$x[,1]
    mean.y <- list_summary_pop$x[,2]
    sd.y <- list_summary_pop$x[,3]

    # sample DB
    results <- NULL
    s.strata <- sample.DB[, var.strata]
    for (i in 1:nrow(list_summary_pop)) {
      Ni <- Nh[i]
      s.ind <- TRUE
      for (k in 1:length(var.strata)) {
        s.ind <- s.ind & (s.strata[,k] == list_summary_pop[i, var.strata[k]])
      }

      yi <- y_sam[s.ind]
      ni <- length(yi)
      mean.sy <- mean(yi)
      mean.sy2 <- round(mean.sy, 3)
      sd.sy2 <- ifelse(ni > 1, round(sd(yi), 3), "-")

      Lower_CI <- mean.sy - za * sd.y[i] * sqrt((Ni - ni)/(Ni - 1)/ni)
      Upper_CI <- mean.sy + za * sd.y[i] *sqrt((Ni - ni)/(Ni - 1)/ni)
      CI <- paste("(", round(Lower_CI, 3), ", ", round(Upper_CI, 3), ")", sep = "")
      if(Ni == 1 | Ni == ni) CI <- "-"

      if(Ni == ni) {
        representi <- "ALL SAMPLED"
      } else {
        represent_ind <- (Lower_CI <= mean.y[i]) & (Upper_CI >= mean.y[i])
        representi <- ifelse(represent_ind == TRUE, "YES", "NO")
      }

      list_summary_pop[i,]$x <- round(list_summary_pop[i,]$x, 3)
      results <- rbind(results, c(as.matrix(list_summary_pop[i,]), ni, mean.sy2, sd.sy2, CI, representi))

      if(i %% 100 == 0) cat(paste(round(i/nrow(list_summary_pop) * 100), "% is done\n", sep = ""))

    }

    results <- as.data.frame(results)
    colnames(results) <- c(var.strata, "N", "pop.mean", "pop.sd","n", "sam.mean", "sam.sd", "CI", "represent")

  }


  if (target == "stage") {

    y_sam <- factor(sample.DB[, "stage"])
    y_pop <- factor(pop.DB[, "stage"])

    list_summary_pop <- aggregate(y_pop, by = apply(pop.DB[, var.strata], 2, function(x) x, simplify = FALSE), FUN = fn_summary_stage)

    Nh <- list_summary_pop$x[,1]
    prob <- list_summary_pop$x[,6:9]

    # sample DB
    results <- NULL
    s.strata <- sample.DB[, var.strata]
    for (i in 1:nrow(list_summary_pop)) {
      Ni <- Nh[i]
      probi <- prob[i, ]
      s.ind <- TRUE
      for (k in 1:length(var.strata)) {
        s.ind <- s.ind & (s.strata[,k] == list_summary_pop[i, var.strata[k]])
      }

      yi <- y_sam[s.ind]
      ni <- length(yi)
      ni.obs <- table(yi)
      ni.pro <- round(prop.table(ni.obs), 3)
      pvalue <- round(fn_multinomial_test(ni.obs, probi), 3)
      d <- round(dist_p(prop.table(ni.obs), probi), 3)

      if(Ni == ni) {
        representi <- "ALL SAMPLED"
      } else {
        represent_ind <- pvalue > 0.05
        representi <- ifelse(represent_ind == TRUE, "YES", "NO")
      }

      list_summary_pop[i,]$x <- round(list_summary_pop[i,]$x, 3)
      results <- rbind(results, c(as.matrix(list_summary_pop[i,]), ni, ni.obs, ni.pro, d, pvalue, representi))

      if(i %% 100 == 0) cat(paste(round(i/nrow(list_summary_pop) * 100), "% is done\n", sep = ""))

    }

    results <- as.data.frame(results)
    colnames(results) <- c(var.strata, "Nh",  "freq1.pop", "freq2.pop", "freq7.pop", "freq9.pop",
                           "prob1.pop", "prob2.pop", "prob7.pop", "prob9.pop",
                           "nh", "freq1.sam", "freq2.sam", "freq7.sam", "freq9.sam",
                           "prob1.sam", "prob2.sam", "prob7.sam", "prob9.sam", "dp", "pvalue", "represent")

  }

  filename <- paste(target, "_", paste("[", c(var.strata), "]", collapse = "_", sep = ""), ".csv", sep = "")
  write.csv(results, filename)
  cat(paste("'", filename, "' is generated.\n", sep = ""))

  return(results)

}

