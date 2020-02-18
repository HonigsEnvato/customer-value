library(lubridate)
library(dplyr)
library(urltools)
library(ggfortify)
library(survival)

########################################################################
## Utility
########################################################################


day_diff <- function(end, start){
    as.integer(difftime(end, start, unit = "days"))
}


hour_diff <- function(end, start){
    as.integer(difftime(end, start, unit = "hour"))
}


compute_mkt_user_type <- function(buyer, author, is_market_user){
    ifelse(!is_market_user, "new",
    ifelse(buyer & author, "both",
    ifelse(buyer & !author, "buyer",
    ifelse(!buyer & author, "author","neither"))))
}


json_to_one_hot <- function(data, json_column){
    ## Flatten the list
    json_list =
        lapply(data[[json_column]], jsonlite::fromJSON) %>%
        lapply(., FUN = function(x) unlist(x)) %>%
        lapply(., FUN = function(x){
            keys = gsub("[[:digit:]]", "", names(x))
            values = gsub("[^[:alpha:]]", "", gsub("[[:blank:]]", "_", tolower(x)))
            paste0(keys, "_", values)
        })
    ## Find all unique combination (columns)
    unique_combo = sort(unique(unlist(json_list)))

    ## Craete one-hot vector for each row
    one_hot = lapply(json_list, FUN = function(x) as.integer(unique_combo %in% x)) %>%
        do.call("rbind", .) %>%
        data.frame %>%
        magrittr::set_colnames(unique_combo)

    ## Drop original json and row bind with the new one-hot data.
    data %>%
        select(-one_of(json_column)) %>%
        cbind(., one_hot)
}


determine_email_account_type <- function(email, cutoff_frequency = 30){
    ## Extract the domains
    domains =
        email %>%
        strsplit(., "@") %>%
        sapply(., function(x) x[[length(x)]]) %>%
        suffix_extract %>%
        pull(domain) %>%
        tolower

    ## Calculate the frequency of each email domain
    domain_frequency = table(domains)

    ## If the frequency is greater than a cutoff frequency, then it is
    ## private. For example, domain like google and yahoo will have a
    ## large frequency.
    personal_email_domain = names(domain_frequency[domain_frequency > cutoff_frequency])

    ## Personal account is determined if the domain is in a personal
    ## email domain determined by frequency
    personal_account = domains %in% personal_email_domain
    personal_account
}


drop_minor_level <- function(data, variable, min_count = 100){
    low_count_level = names(which(table(data[[variable]]) < min_count))
    data %>%
        filter(., !(data[[variable]] %in% low_count_level)) %>%
        droplevels
}


country_to_geo <- function(country_code){
    ## hard coded geo code according to the market dashboard
    ##
    ## https://reports.envato.com/#/views/MarketDashboard/Information?:iid=2
    tier_one_country_code = c("AU", "CA", "GB", "US")
    latam = c("CL", "PE", "MX", "AR", "CO", "EC", "PR", "DO", "PA", "GT", "BO")
    tier_two_country_code = c("BR", "FR", "DE", "IN", "IT", "JP", "NL", "RU", "KR", "ES", "TR", latam)

    ifelse(country_code %in% tier_one_country_code, "GEO Tier 1",
    ifelse(country_code  %in% tier_two_country_code, "GEO Tier 2", "GEO Tier 3"))
}


is_promotional_plan <- function(plan_name){
    as.integer(!grepl("unlimited", plan_name))
}


is_freelancer <- function(available_for_freelance){
    ifelse(is.na(available_for_freelance), "Unknown",
    ifelse(available_for_freelance == 1, "yes", "no")) %>%
        as.factor
}


########################################################################
## Chunr Modelling
########################################################################

split_train_test <- function(data, train_pct){
    sample_size = NROW(data)
    train_size = floor(sample_size * train_pct)
    train_ind = sample(sample_size, size = train_size)
    list(train = data[train_ind, ], test = data[-train_ind, ])
}


subset_model_coef <- function(data, size, formula){
    data %>%
        sample_n(., size = size) %>%
        coxph(formula, data = ., control = model_control) %>%
        coef
}


bootstrap_model_coef <- function(data, sizes, formula){
    coef_matrix =
        lapply(sizes, FUN = function(x){
            subset_model_coef(data = data, size = x, formula = formula)
        }) %>%
        do.call("rbind", .) %>%
        cbind(., sample_size = sizes) %>%
        data.frame
    coef_matrix
}


ensemble <- function(task, learner_name, test_set){
    learners = lapply(learner_name,
                      FUN = function(x) makeLearner(x, predict.type = "prob",
                                                    fix.factors.prediction = TRUE))
    stacked = makeStackedLearner(base.learners = learners,
                                 super.learner = "classif.xgboost",
                                 predict.type = "prob")
    model = train(stacked, task)
    predicted = predict(model, newdata = test_set)
}


predictive_summary <- function(y, pred, plotRoc = FALSE){
    n = length(pred)
    confusion_matrix = table(actual = y, predicted = pred)

    ## Show the confusion matrix
    cat("Confusion Matrix:\n\n")
    print(confusion_matrix)
    cat("\n\n")

    ## Confusion matrix in percentage, so it's easier to see the
    ## specificty and sensitivity
    cat("Confusion Matrix (Pct):\n\n")
    confusion_matrix_pct = round(confusion_matrix/n, 2)
    print(confusion_matrix_pct)
    cat("\n\n")


    ## Calculate and show the classification error
    classification_error = (n - sum(diag(confusion_matrix)))/n
    cat(glue::glue("Classification Error: {round(classification_error, 4) * 100}% \n\n"))

    ## NOTE (Michael): This only works for
    if(inherits(pred, "Prediction") & plotRoc){
        result = generateThreshVsPerfData(pred, measures = list(fpr, tpr, mmce))
        plotROCCurves(result)
    }
}


########################################################################
## Lifetime value
########################################################################


## define function to calculate LTV
compute_ltv <- function(object, risk = NULL, plan_price = 33, net_percentage = 0.5){
    if(inherits(object, "coxph")){
        if(is.null(risk)){
            term_risks = compute_term_risk(object)
            risk = term_risks$predicted_risk
        }

        ## Create the survival curve object
        fit = survfit(object)

        ## Start with cumulative hazard function since this is how you can
        ## apply the proportional hazard.
        h_t = fit$cumhaz

        ## Create the cumulative hazard matrix, each column corresponds to
        ## each individual risk lambda(t).
        survival_matrix = compute_term_survival_matrix(cumhaz = h_t, risk = risk)

        ## Calculate the delta S(t)
        prop_survived_matrix = apply(survival_matrix, 2, FUN = function(x) abs(diff(c(1, x, 0))))

        ## Calculate the expected survival period.
        survival_period = matrix(c(fit$time, max(fit$time)), nr = 1) %*% prop_survived_matrix

        ## Calculate LTV, which is the expected survival period multiplied
        ## by the subscription plan.
        ltv = c(survival_period * plan_price)

    } else if(inherits(object, "survfit")){
        if(length(object$n) == 1){
            prop_survived = abs(diff(c(1, object$surv, 0)))
            expected_survival_time = sum(prop_survived * c(object$time, max(object$time)))
            ltv = expected_survival_time * plan_price
        } else {
            ltv =
                sapply(seq(1, length(object$n)),
                       FUN = function(x){
                           compute_ltv(object = object[x], plan_price = plan_price, net_percentage = 1)
                       })
            names(ltv) = names(object$strata)
        }
    } else if(is.vector(object)){
        ## NOTE (Michael): Assumes a survival curve with index at 0.
        ml = length(object) - 1
        ltv = sum(abs(diff(c(1, object, 0))) * c(0:ml, ml)) * plan_price
    }
    ## Net percentage is the percentage of the gross revenue after
    ## accounting for author cuts
    ltv * net_percentage
}


compute_term_risk <- function(model, nsd = 2){
    unique_risk_levels = expand.grid(model$xlevels)
    predicted_risk = predict(model, newdata = unique_risk_levels, type = "risk", se.fit = TRUE)
    data.frame(unique_risk_levels,
               predicted_risk = predicted_risk$fit,
               predicted_risk_upper = predicted_risk$fit + nsd * predicted_risk$se.fit,
               predicted_risk_lower = predicted_risk$fit - nsd * predicted_risk$se.fit)
}


compute_term_survival_matrix <- function(cumhaz, risks){
    hazard_matrix = matrix(cumhaz, nc = 1) %*% matrix(risks, nc = length(risks))
    survival_matrix = exp(-hazard_matrix)
    survival_matrix
}




########################################################################
## Data processing and LTV calculation wrapper
########################################################################

calculate_monthly_ltv <- function(data,
                                  group_var,
                                  plan_price,
                                  net_percentage){
    form = formula(sprintf("Surv(net_payments, churned) ~ %s", paste0(group_var, collapse = " + ")))
    model = coxph(form, data = data)
    compute_ltv(model, plan_price = plan_price, net_percentage = net_percentage) %>%
        data.frame(expand.grid(model$xlevels), ltv = ., stringsAsFactors = FALSE)
}


calculate_annual_ltv <- function(data,
                                 group_var,
                                 plan_price,
                                 net_percentage,
                                 fallback = FALSE){
    annual_ltv = try({
        calculate_monthly_ltv(data = data, group_var = group_var, plan_price = plan_price,
                              net_percentage = net_percentage)
    })

    if(inherits(annual_ltv, "try-error")){
        if(fallback){
            annual_ltv =
                data %>%
                group_by(!!!syms(group_var)) %>%
                summarise(retention_rate = 1 - mean(churned, na.rm = TRUE)) %>%
                mutate(ltv = (1 + retention_rate) * plan_price * net_percentage) %>%
                select(-retention_rate) %>%
                data.frame
        } else {
            stop("Annual LTV can not be calculated")
        }
    }
    annual_ltv
}

## The following two function are temporary solutions. We take the
## annual for upgrader and monthly for downgrader.
calculate_upgrader_ltv <- function(annual_ltv){return(annual_ltv)}
calculate_downgrader_ltv <- function(monthly_ltv){return(monthly_ltv)}

calculate_breakdown_ltv <- function(payment_data,
                                    breakdown_data,
                                    plan_type_col = "plan_type",
                                    use_in_calculation_col = "use_in_ltv_calculation",
                                    monthly_plan_name = "monthly",
                                    annual_plan_name = "annual",
                                    upgrader_plan_name = "upgrader",
                                    downgrader_plan_name = "downgrader",
                                    monthly_plan_price = 33,
                                    annual_plan_price = 198,
                                    net_percentage = 0.485,
                                    min_count = 100,
                                    expand_group = TRUE,
                                    fallback_annual = FALSE){

    ## Extract the column informations
    user_col = intersect(colnames(payment_data), colnames(breakdown_data))
    joined_set = inner_join(payment_data, breakdown_data, by = user_col)
    group_var = colnames(breakdown_data)[colnames(breakdown_data) != user_col]


    ## Drop minor levels
    complete_set = Reduce(f = function(x, y){
        drop_minor_level(x, variable = y, min_count = min_count)
    }, x = group_var, init = joined_set)

    ## Monthly LTV
    monthly_ltv =
        complete_set %>%
        filter(!!sym(plan_type_col) == monthly_plan_name) %>%
        filter(!!sym(use_in_calculation_col) == 1) %>%
        calculate_monthly_ltv(.,
                              group_var = group_var,
                              plan_price = monthly_plan_price,
                              net_percentage = net_percentage) %>%
        mutate(!!sym(plan_type_col) := monthly_plan_name)

    ## Annual LTV
    annual_ltv =
        complete_set %>%
        filter(!!sym(plan_type_col) == annual_plan_name) %>%
        filter(!!sym(use_in_calculation_col) == 1) %>%
        calculate_annual_ltv(.,
                             group_var = group_var,
                             plan_price = annual_plan_price,
                             net_percentage = net_percentage,
                             fallback = fallback_annual) %>%
        mutate(!!sym(plan_type_col) := annual_plan_name)

    ## Upgrader LTV
    upgrader_ltv =
        calculate_upgrader_ltv(annual_ltv) %>%
        mutate(!!sym(plan_type_col) := upgrader_plan_name)

    ## Downgrader LTV
    downgrader_ltv =
        calculate_downgrader_ltv(monthly_ltv) %>%
        mutate(!!sym(plan_type_col) := downgrader_plan_name)

    ## Calculate the number of subscriber for each group for LTV
    ## weighting.
    ##
    ## The grouping variables are expanded (create all possible
    ## combination) if expand_group is TRUE.
    if(expand_group){
        ## This part expand the combination of the plan types and also
        ## the group variables.
        all_plan_types = list(c(monthly_plan_name, annual_plan_name,
                                upgrader_plan_name, downgrader_plan_name))
        expanded_groups =
            breakdown_data %>%
            select(-!!sym(user_col)) %>%
            apply(., 2, function(x) unique(x)) %>%
            c(., plan_type = all_plan_types) %>%
            expand.grid(., stringsAsFactors = FALSE)

        ## We then create the subscriber count for each group with the
        ## subscriber of the expanded group set to zero.
        group_subscriber =
            joined_set %>%
            mutate(subscriber = 1) %>%
            right_join(expanded_groups) %>%
            tidyr::replace_na(., list(subscriber = 0)) %>%
            group_by(!!!syms(c(plan_type_col, group_var))) %>%
            summarise(subscriber = sum(subscriber))
    } else {
        group_subscriber =
            joined_set %>%
            group_by(!!!syms(c(plan_type_col, group_var))) %>%
            summarise(subscriber = n())
    }

    ## Combine the results
    combined_ltv = rbind(monthly_ltv, annual_ltv, upgrader_ltv, downgrader_ltv)
    combined_ltv %>%
        right_join(group_subscriber)
}
