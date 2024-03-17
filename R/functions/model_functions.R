##########################################
####  Functions for modelling
##########################################
#### | Project name: Atra climate
#### | Script type: Data processing
#### | What it does: Description

##########################################



# Functions list ----------------------------------------------------------
#### 1: prepare_model_data() - more abstract functions: create_pseudoabsences(); create_blocks()
#### 2: fit_models()
#### 3: project_models()
#### 4: ensemble_projections()


project_single_models <- function(caret_model_list, raster_to_predict)
{
  
  data_to_predict <- getValues(raster_to_predict)
  raster_mask <- raster_to_predict[[1]]
  
  #### Predict single models
  # GLM, GAM, GBM, RF
  mod_glm <- caret::predict.train(object = caret_model_list$glm, data_to_predict, 
                                  type = "prob", na.action = "na.pass")
  mod_gam <- caret::predict.train(object = model_list$gam, data_to_predict, 
                                  type = "prob", na.action = "na.pass")
  mod_gbm <- caret::predict.train(object = caret_model_list$gbm, data_to_predict, 
                                  type = "prob", na.action = "na.pass")
  mod_rf <- predict(object = caret_model_list$rf, data_to_predict, 
                    type = "prob", na.action = "na.omit")
  #
  mod_ann <- caret::predict.train(object = caret_model_list$nnet, data_to_predict, 
                                  type = "prob", na.action = "na.pass")
  mod_ada <- caret::predict.train(object = caret_model_list$ada, data_to_predict, 
                                  type = "prob", na.action = "na.pass")
  mod_maxnet <- caret::predict.train(object = model_list$glmnet, data_to_predict, 
                                     type = "prob", na.action = "na.pass")
  mod_mars <- caret::predict.train(object = model_list$bagEarth, data_to_predict, 
                                   type = "prob", na.action = "na.pass")
  
  
  #### Convert to raster
  pred_glm <- setValues(raster_mask, mod_glm[, 2])
  pred_gam <- setValues(raster_mask, mod_gam[, 2])
  pred_gbm <- setValues(raster_mask, mod_gbm[, 2])
  
  
  pred_ann <- setValues(raster_mask, mod_ann[, 2])
  pred_ada <- setValues(raster_mask, mod_ada[, 2])
  pred_maxnet <- setValues(raster_mask, mod_maxnet[, 2])
  pred_mars <- setValues(raster_mask, mod_mars[, 1])
  #### Mask raster
  
  pred_glm[is.na(raster_mask)] <- NA
  
  pred_rf <- pred_glm
  pred_rf[!is.na(pred_rf)] <- mod_rf[, 2]
  pred_gam[is.na(raster_mask)] <- NA
  pred_gbm[is.na(raster_mask)] <- NA
  pred_rf[is.na(raster_mask)] <- NA
  
  pred_ann[is.na(raster_mask)] <- NA
  pred_ada[is.na(raster_mask)] <- NA
  pred_maxnet[is.na(raster_mask)] <- NA
  pred_mars[is.na(raster_mask)] <- NA
  
  r_stack <- stack(pred_glm, pred_gam, pred_gbm, pred_rf,
                   pred_ann, pred_ada, pred_maxnet, pred_mars)
  names(r_stack) <- c("glm", "gam", "gbm", "rf", "ann", "ada", "maxent", "mars")
  
  
  return(r_stack)
  
}







############################################



#' Clean model evaluations
#'
#' @param x output of biomod get_evaluations
#'
#' @return
#' @export
#'
#' @examples
#' 
clean_model_evaluations <- function(x, ensemble = FALSE)
{
  
  
  if (ensemble)
  {
    var_names <- names(x) %>% 
      str_remove("Salamandra.atra_") %>% 
      str_remove("Salamandra.atra.north_") %>%
      str_remove("Salamandra.atra.south_") %>%
      str_remove("_AllData")
    
    evals_ens <- x %>% 
      map(t) %>% 
      reduce(rbind) %>% 
      as.data.frame() %>% 
      rownames_to_column() %>% 
      filter(str_detect(rowname,"Testing.data")) %>% 
      mutate(
        fold = var_names
      ) %>% 
      separate(fold, sep = "_", into = c("algorithm", "waste", "fold")) %>% 
      transmute(algorithm, fold, TSS, AUC = ROC)
    return(evals_ens)
  }
  
  tss_evals <- x["TSS","Testing.data",,,] %>% 
    as.data.frame() %>% 
    rownames_to_column("algorithm") %>% 
    pivot_longer(2:ncol(.)) %>% 
    transmute(
      algorithm,
      fold = name, TSS = value,
      bycode = str_c(algorithm, "_", fold)
    )
  
  auc_evals <- x["ROC","Testing.data",,,] %>% 
    as.data.frame() %>% 
    rownames_to_column("algorithm") %>% 
    pivot_longer(2:ncol(.)) %>% 
    transmute(
      algorithm,
      fold = name, AUC = value,
      bycode = str_c(algorithm, "_", fold)
    )  
  
  xout <- inner_join(tss_evals, auc_evals, by = "bycode") %>% 
    transmute(
      algorithm = algorithm.x,
      fold = fold.x,
      TSS, AUC
    )
  return(xout)
  
}
