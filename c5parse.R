# preproc -----
make_df_c5 <- function(x){
  names(x) <- gsub(" ", "_", names(x))
  cs <- unname(which(sapply(x, function(x){is.character(x)|is.factor(x)})))
  for (c in cs) {
    x[[c]] <- gsub(" ", "_", x[[c]])
    x[[c]] <- gsub("=", "eq", x[[c]])
    x[[c]] <- as.factor(x[[c]])
  }

  x
}

# parser -----
parse_row <- function(x){
  x <- strsplit(x, " ")
  x <- lapply(x,
              function(y){
                y <- strsplit(y, "=")
                nms <- unlist(lapply(y, function(z) z[1]))
                val <- unlist(lapply(y, function(z) z[2]))
                names(val) <- nms
                val
              })

  unlist(x)
}

type12 <- function(x){
  x <- gsub("\"", "", x)
  att_loc <- regexpr("att=", x)
  cut_loc <- regexpr("cut=", x)
  rslt_loc <- regexpr("result=", x)
  val_loc <- regexpr("val=", x)

  var <- val <- rslt <- rep(NA, length(x))

  cat_vals <- (cut_loc < 1 & val_loc > 0)

  if (any(cat_vals)) {
    var[cat_vals] <- substring(x[cat_vals], att_loc[cat_vals] + 4, val_loc[cat_vals] - 2)
    val[cat_vals] <- substring(x[cat_vals], val_loc[cat_vals] + 4, length(cat_vals))
    rslt[cat_vals] <- "="  
  }
  if (any(!cat_vals)) {
    var[!cat_vals] <- substring(x[!cat_vals], att_loc[!cat_vals] + 4, cut_loc[!cat_vals] - 2)        
    val[!cat_vals] <- substring(x[!cat_vals], cut_loc[!cat_vals] + 4, rslt_loc[!cat_vals] - 1)
    val[!cat_vals] <- as.numeric(val[!cat_vals])
    rslt[!cat_vals] <- substring(x[!cat_vals], rslt_loc[!cat_vals] + 7)
    rslt[!cat_vals] <- ifelse(rslt[!cat_vals] == "<", "<=", rslt[!cat_vals])
  }

  list(var = var, val = val, rslt = rslt, text = paste(var, rslt, val))
}

type3 <- function(x){
  x <- gsub("\"", "", x)
  att_loc <- regexpr("att=", x)
  elts_loc <- regexpr("elts=", x)
  var <- substring(x, att_loc + 4, elts_loc - 2)
  val <- substring(x, elts_loc + 5)
  multi_vals <- grepl(",", val)
  val <- gsub(",", "|", val)
  val <- ifelse(multi_vals, paste("{", val, "}", sep = ""), val)
  txt <- ifelse(multi_vals, paste(var, "in", val),  paste(var, "=", val))

  list(var = var, val = val, text = txt)
}

conds <- function(x){
  x <- gsub("\"", "", x)
  cond_loc <- regexpr("conds=", x)
  cover_loc <- regexpr("cover=", x)
  ok_loc <- regexpr("ok=", x)
  lift_loc <- regexpr("lift=", x)
  class_loc <- regexpr("class=", x)

  cond <- as.numeric(substring(x, cond_loc + 6, cover_loc - 2))
  cover <- as.numeric(substring(x, cover_loc + 6, ok_loc - 2))
  ok <- as.numeric(substring(x, ok_loc + 3, lift_loc - 2))
  lift <- as.numeric(substring(x, lift_loc + 5, class_loc - 2))
  cls <- substring(x, class_loc + 6)

  list(cond = cond, cover = cover, ok = ok, lift = lift, cls = cls)
}

parse_c5 <- function(x){
  x <- strsplit(x, "\n")[[1]]
  # rules... <- committee
  # conds... <- rule
  # types... <- condition

  com_num <- rule_num <- cond_num <- rep(NA, length(x))
  com_idx <- rule_idx <- cond_idx <- 0

  for (i in seq_along(x)) {
    tt <- parse_row(x[i])

    # start of new committee
    if (names(tt)[1] == "rules") {
      com_idx <- com_idx + 1
      rule_idx <- 0
    }
    com_num[i] <- com_idx

    # start of new rule
    if (names(tt)[1] == "conds") {
      rule_idx <- rule_idx + 1
      cond_idx <- 0
    }
    rule_num[i] <-rule_idx

    # individual conditions
    if(names(tt)[1] == "type")
    {
      cond_idx <- cond_idx + 1
      cond_num[i] <- cond_idx
    }
  }

  # number of committee, each committee start with "rules"...
  num_com <- sum(grepl("^rules=", x))
  rules_per_com <- unlist(lapply(split(rule_num, as.factor(com_num)), max))
  rules_per_com <- rules_per_com[rules_per_com > 0]
  if (!is.null(rules_per_com) && num_com > 0) {
    names(rules_per_com) <- paste("Com", 1:num_com)
  }

  # parse rule performance
  is_new_rule <- grepl("^conds=", x)
  split_conds <- rep(NA, length(x))
  split_cover <- rep(NA, length(x))
  split_ok <- rep(NA, length(x))
  split_lift <- rep(NA, length(x))
  split_class <- rep(NA, length(x))
  if (any(is_new_rule)) {
    split_conds[is_new_rule] <- conds(x[is_new_rule])$cond
    split_cover[is_new_rule] <- conds(x[is_new_rule])$cover
    split_ok[is_new_rule] <- conds(x[is_new_rule])$ok
    split_lift[is_new_rule] <- conds(x[is_new_rule])$lift
    split_class[is_new_rule] <- conds(x[is_new_rule])$cls
  }

  # parse rules
  split_var <- rep("", length(x))
  split_val <- rep(NA, length(x))
  split_cats <- rep("", length(x))
  split_dir <- rep("", length(x))

  # type="2" att="nox" cut="0.66799998" result=">"
  # nox > 0.668

  is_type12 <- (grepl("^type=\"2\"", x) | grepl("^type=\"1\"", x))
  if (any(is_type12)) {
    split_var[is_type12] <- type12(x[is_type12])$var
    split_var[is_type12] <- gsub("\"", "", split_var[is_type12])
    split_dir[is_type12] <- type12(x[is_type12])$rslt
    split_val[is_type12] <- type12(x[is_type12])$val
  }

  # type="3" att="X4" elts="c","d"
  # X4 in {c, d}

  is_type3 <- grepl("^type=\"3\"", x)
  if (any(is_type3)) {
    split_var[is_type3] <- type3(x[is_type3])$var
    split_cats[is_type3] <- type3(x[is_type3])$val
    split_cats[is_type3] <- gsub("[{}]", "", split_cats[is_type3])
    split_cats[is_type3] <- gsub("\"", "", split_cats[is_type3])
    split_cats[is_type3] <- gsub(" ", "", split_cats[is_type3])
  }

  if(!any(is_type12) & !any(is_type3)) return(NULL)

  split_rules <- data.frame(committee = com_num,
                            rule = rule_num,
                            variable = split_var,
                            dir = split_dir,
                            value = split_val,
                            category = split_cats)
  split_rules <- split_rules[split_rules$variable != "",]

  split_metrics <- data.frame(committee = com_num,
                              rule = rule_num,
                              conditions = split_conds,
                              cover = split_cover,
                              default_class = split_ok,
                              lift = split_lift,
                              class = split_class)
  split_metrics <- split_metrics[!is.na(split_metrics$conditions),]

  res <- list()
  res[["metrics"]] <- split_metrics
  res[["rules"]] <- split_rules
  res[["default_class"]] <- gsub("\"", "", parse_row(x[3])[["default"]])
  res
}
