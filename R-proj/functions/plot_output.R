
plot_age_fit_from_params = function(params, params_names, params_base, params_temporal, init_state, fit = c("cases", "inf", "hosp", "deaths", "negtests"), kc_data, start_date = NULL, end_date = NULL)
{
  parameters = get_params(params, params_names, params_base)
  parameters_temporal = get_temporal_params(params, params_names, params_temporal)

  if (is.null(start_date)) { start_date = get_date_from_model_day(parameters$first_inf_day, parameters$model_day0_date) }
  if (is.null(end_date)) { end_date = max(kc_data$date) }

  model_out = run_model_by_date(parameters, parameters_temporal, init_state, start_date, end_date)
  out_data = shape_data_wide(shape_data_long(model_out, parameters$model_day0_date))
  
  plot_age_fit(out_data, fit, kc_data, start_date, end_date)
}

plot_age_fit_from_param_set = function(param_matrix, params_names, params_base, params_temporal, init_state, fit = c("cases", "hosp", "deaths", "negtests"), kc_data, start_date = NULL, end_date = NULL)
{
  model_data = get_model_data_param_sets(param_matrix, params_names, params_base, params_temporal, 
                                         init_state, kc_data, start_date, end_date)
  plot_age_fit_from_data_set(model_data, fit, kc_data, start_date, end_date)
}

plot_age_fit_from_data_set = function(data_set, fit = c("cases", "inf", "hosp", "deaths", "negtests"), kc_data, 
                                      start_date = NULL, end_date = NULL, calibration_date = NULL,
                                      plot_age_separate = FALSE, plot_data_as = "l")
{
  if (is.null(start_date))
  {
    start_date = max( min(do.call(c, lapply(data_set, function(x) min(x$date)))), min(kc_data$date) )
  }
  if (is.null(end_date))
  {
    end_date = min(max(do.call(c, lapply(data_set, function(x) max(x$date)))), max(kc_data$date))
  }
  if (is.null(calibration_date))
  {
    pch = 16
  } else
  {
    pch = c(rep(16, sum(which(kc_data$date <= calibration_date))),
            rep(1, sum(which(kc_data$date > calibration_date))))
  }
  
  # set columns and labels
  model_cols = get_model_col_from_metric(fit, by_age = TRUE)
  data_cols = get_data_col_from_metric(fit, by_age = TRUE)
  label = get_label_from_metric(fit)
  smooth_cols = paste0("ma_", data_cols)
  
  # TODO could calculate max for ylim from the model, but for now just base off off real data
  x_lim = c(start_date, end_date)
  y_lim = c(0, 1.5 * max( kc_data[kc_data$date >= start_date & kc_data$date <= end_date, data_cols], na.rm = TRUE ))
  
  plot(x = x_lim, y = y_lim, type = "n", las = 1, xlab = "", ylab = paste("Daily", label),
       xlim = x_lim, ylim = y_lim)
  
  # note the drop = TRUE is an annoyance of tibbles which return nx1 matrix not vector
  for (i in 1:length(data_set))
  {
    lines(x = data_set[[i]]$date, y = data_set[[i]][,model_cols[1], drop = TRUE], lty = 1, lwd = 0.5, col = alpha("black", 0.25))
  }
  if (plot_data_as == "l")
  {
    lines(x = kc_data$date, y = kc_data[,smooth_cols[1]], lty = 1, lwd = 3, col = "black")
  }  else if (plot_data_as == "p")
  {
    points(x = kc_data$date, y = kc_data[,smooth_cols[1]], col = "black", cex = 1.5, pch = pch)
  }
  
  # age 2
  if (plot_age_separate)
  {
    plot(x = x_lim, y = y_lim, type = "n", las = 1, xlab = "", ylab = paste("Daily", label), xlim = x_lim, ylim = y_lim)
  }
  for (i in 1:length(data_set))
  {
    lines(x = data_set[[i]]$date, y = data_set[[i]][,model_cols[2], drop = TRUE], lty = 1, lwd = 0.5, col = alpha("darkblue", 0.25))
  }
  if (plot_data_as == "l")
  {
    lines(x = kc_data$date, y = kc_data[,smooth_cols[2]], lty = 1, lwd = 3, col = "darkblue")
  } else if (plot_data_as == "p")
  {
    points(x = kc_data$date, y = kc_data[,smooth_cols[2]], col = "darkblue", cex = 1.5, pch = pch)
  }

  # age 3
  if (plot_age_separate)
  {
    plot(x = x_lim, y = y_lim, type = "n", las = 1, xlab = "", ylab = paste("Daily", label), xlim = x_lim, ylim = y_lim)
  }
  for (i in 1:length(data_set))
  {
    lines(x = data_set[[i]]$date, y = data_set[[i]][,model_cols[3], drop = TRUE], lty = 1, lwd = 0.5, col = alpha("darkgreen", 0.25))
  }
  if (plot_data_as == "l")
  {
    lines(x = kc_data$date, y = kc_data[,smooth_cols[3]], lty = 1, lwd = 3, col = "darkgreen")
  }  else if (plot_data_as == "p")
  {
    points(x = kc_data$date, y = kc_data[,smooth_cols[3]], col = "darkgreen", cex = 1.5, pch = pch)
  }
  
  # age 4
  if (plot_age_separate)
  {
    plot(x = x_lim, y = y_lim, type = "n", las = 1, xlab = "", ylab = paste("Daily", label), xlim = x_lim, ylim = y_lim)
  }
  for (i in 1:length(data_set))
  {
    lines(x = data_set[[i]]$date, y = data_set[[i]][,model_cols[4], drop = TRUE], lty = 1, lwd = 0.5, col = alpha("darkred", 0.25))
  }
  if (plot_data_as == "l")
  {
    lines(x = kc_data$date, y = kc_data[,smooth_cols[4]], lty = 1, lwd = 3, col = "darkred")
  }  else if (plot_data_as == "p")
  {
    points(x = kc_data$date, y = kc_data[,smooth_cols[4]], col = "darkred", cex = 1.5, pch = pch)
  }
  
  legend("topleft", legend = c("0-19","20-49","50-69","70+"), col = c("black","darkblue","darkgreen","darkred"), lty = 1, lwd = 2, bty = "n")
  
  if (!is.null(calibration_date) & plot_data_as == "p")
  {
    legend("top", legend = c("calibration", "validation"), pch = c(16, 1), bty = "n")
  }
  
  if (fit == "inf")
  {
    legend("topright", legend = NA, title = "Data showing cases", bty = "n")
  }
  
}

# plot fit by age for specified param
# assume model already run and data transformed (can run wrapper with params if desired)
plot_age_fit = function(out_data, fit = c("cases", "inf", "hosp", "deaths", "negtests"), kc_data, start_date = NULL, end_date = NULL)
{
  if (is.null(start_date))
  {
    start_date = max(min(out_data$date), min(kc_data$date))
  }
  if (is.null(end_date))
  {
    end_date = min(max(out_data$date), max(kc_data$date))
  }

  model_cols = get_model_col_from_metric(fit, by_age = TRUE)
  data_cols = get_data_col_from_metric(fit, by_age = TRUE)
  label = get_label_from_metric(fit)
  smooth_cols = paste0("ma_", data_cols)

  
  x_lim = c(start_date, end_date)
  y_lim = c(0, max( out_data[out_data$date >= start_date & out_data$date <= end_date, model_cols], 
                    kc_data[kc_data$date >= start_date & kc_data$date <= end_date, data_cols], na.rm = TRUE ))

  plot(x = x_lim, y = y_lim, type = "n", las = 1, xlab = "",ylab = paste("Daily", label),
       xlim = x_lim, ylim = y_lim)
  
  # note the drop = TRUE is an annoyance of tibbles which return nx1 matrix not vector
  lines(x = kc_data$date, y = kc_data[,smooth_cols[1]], lty = 2, lwd = 2, col = "gray30")
  lines(x = out_data$date, y = out_data[,model_cols[1], drop = TRUE], lty = 1, lwd = 1.5, col = "black")
  points(x = kc_data$date, y = kc_data[,data_cols[1]], col = "gray30", pch = 1, cex=0.5)
  
  lines(x = kc_data$date, y = kc_data[,smooth_cols[2]], lty = 2, lwd = 2, col = "lightblue")
  lines(x = out_data$date, y = out_data[,model_cols[2], drop = TRUE], lty = 1, lwd = 1.5, col = "darkblue")
  points(x = kc_data$date, y = kc_data[,data_cols[2]], col = "lightblue", pch = 1, cex=0.5)
  
  lines(x = kc_data$date, y = kc_data[,smooth_cols[3]], lty = 2, lwd = 2, col = "lightgreen")
  lines(x = out_data$date, y = out_data[,model_cols[3], drop = TRUE], lty = 1, lwd = 1.5, col = "darkgreen")
  points(x = kc_data$date, y = kc_data[,data_cols[3]], col = "lightgreen", pch = 1, cex=0.5)
  
  lines(x = kc_data$date, y = kc_data[,smooth_cols[4]], lty = 2, lwd = 2, col = "pink")
  lines(x = out_data$date, y = out_data[,model_cols[4], drop = TRUE], lty = 1, lwd = 1.5, col = "darkred")
  points(x = kc_data$date, y = kc_data[,data_cols[4]], col = "pink", pch = 1, cex=0.5)
  legend("topleft", legend = c("0-19","20-49","50-69","70+"), col = c("black","darkblue","darkgreen","darkred"), lty = 1, lwd = 2, bty = "n")
  
  if (fit == "inf")
  {
    legend("topright", legend = NA, title = "Data showing cases", bty = "n")
  }
}

# plot calibrated sd values from matrix of parameter sets
plot_sd_from_param_set = function(param_matrix, param_names, sd_init_date, sd_full_date)
{
  sd_idxs = grep("sd_", param_names)
  last_idxs = sapply(param_names[sd_idxs], function(x) max(unlist(gregexpr(pattern = '_', x))))
  sd_suffixs = sapply(1:length(sd_idxs), function(x) substr(param_names[sd_idxs[x]], last_idxs[x] +1, nchar(param_names[sd_idxs[x]])))
  sd_suffixs = unique(sd_suffixs)
  
  
  sd_params = sapply(sd_suffixs, function(x) grep(paste0("sd_", x), param_names))
  
  sd_dates = c(sd_init_date - 7, sd_full_date) # 0 starts 7 days before with ramping
  for (i in 1:nrow(sd_params))
  {
    if (startsWith(param_names[sd_params[i,1]], "t-"))
    {
      param_components = strsplit(param_names[sd_params[i,1]], "-", fixed = TRUE)[[1]] # list length of char vector, we only have 1
      param_date = ymd(param_components[2]) # first is t
      sd_dates = c(sd_dates, param_date)
    }
  }
  sd_init = 0

  cols = c("black","darkblue","darkgreen","darkred")
  for (age in 1:length(sd_suffixs))
  {
    plot(0, 0, xlim = range(sd_dates), ylim = c(0, 1), xlab = "", ylab = paste("sd age", sd_suffixs[age]), bty = "l", xaxt = "n")
    axis(1, at = sd_dates, labels = sd_dates, las = 2)
    
    for (r in 1:nrow(param_matrix))
    {
      lines(sd_dates, c( sd_init, param_matrix[r, sd_params[,age]]), lwd = 0.5, col = alpha(cols[age], 0.25))
    }
  }
  # legend("topleft", legend = c("0-19","20-49","50-69","70+"), col = cols, lty = 1, lwd = 2, bty = "n")
}

plot_sd_from_long_set = function(out_long_list, start_date = NULL, end_date = NULL, title = NULL)
{
  if (is.null(start_date))
  {
    start_date = min(do.call(c, lapply(out_long_list, function(x) min(x$date))))
  }
  if (is.null(end_date))
  {
    end_date = max(do.call(c, lapply(out_long_list, function(x) max(x$date))))
  }
  
  x_lim = c(start_date, end_date)
  cols = c("black","darkblue","darkgreen","darkred") 
  
  plot(x = x_lim, y = c(0,1), type = "n", las = 1, xlab = "", ylab = "Social distancing",
       xlim = x_lim, ylim = c(0,1), xaxt = "n" )
  draw_month_axis(start_date, end_date)
  
  for (i in 1:length(out_long_list))
  {
    data = filter(out_long_list[[i]], state == "sd") %>%
      group_by(time, date, age) %>%
      summarise(value = first(value), .groups = "drop") 
    
    for (a in na.omit(unique(out_long_list[[1]]$age)))
    {
      data_age = filter(data, age == a)
      lines(data_age$date, data_age$value, lty = 1, lwd = 0.5, col = alpha(cols[a], 0.5))
    }
  }
}

# plot vaccination coverage from model runs from cum_vac total
plot_cum_vac_from_long_set = function(out_long_list, total_pop, by_age = TRUE, start_date = NULL, end_date = NULL, title = NULL)
{
  
  if (is.null(start_date))
  {
    start_date = min(do.call(c, lapply(out_long_list, function(x) min(x$date))))
  }
  if (is.null(end_date))
  {
    end_date = max(do.call(c, lapply(out_long_list, function(x) max(x$date))))
  }
  
  x_lim = c(start_date, end_date)
  cols = if (by_age) { c("black","darkblue","darkgreen","darkred") } else { c("black") }

  plot(x = x_lim, y = c(0,1), type = "n", las = 1, xlab = "", ylab = "Vaccine coverage",
       xlim = x_lim, ylim = c(0,1), xaxt = "n" )
  draw_month_axis(start_date, end_date)
  
  for (i in 1:length(out_long_list))
  {
    data = filter(out_long_list[[i]], state == "cum_vac")
    if (by_age)
    {
      for (a in na.omit(unique(out_long_list[[1]]$age)))
      {
        data_age = filter(data, age == a)
        lines(data_age$date, data_age$value / total_pop[a], lty = 1, lwd = 0.5, col = alpha(cols[a], 0.5))
      }
    }
    else
    {
      # sum across ages
      data = data %>%
        group_by(time, date, vac) %>%
        summarise(value = sum(value), .groups = "drop") 
      lines(data$date, data$value / total_pop, lty = 1, lwd = 0.5, col = alpha(cols, 0.5))
    }
  }
  if (!is.null(title))
  {
    mtext(title, side = 3, line = 0.5)
  }
  
  if (by_age) { legend("topleft", legend = c("0-19","20-49","50-69","70+"), col = cols, lty = 1, lwd = 2, bty = "n") }
}

# plot vaccination coverage from model runs by summing across compartments
plot_vac_across_compartments_from_long_set = function(out_long_list, total_pop, vac_names, by_age = TRUE, sum_vacs = FALSE,
                                                      start_date = NULL, end_date = NULL, title = NULL)
{
  if (is.null(start_date))
  {
    start_date = min(do.call(c, lapply(out_long_list, function(x) min(x$date))))
  }
  if (is.null(end_date))
  {
    end_date = max(do.call(c, lapply(out_long_list, function(x) max(x$date))))
  }
  
  x_lim = c(start_date, end_date)
  cols = if (by_age) { c("black","darkblue","darkgreen","darkred") } else { c("black") }
  
  plot(x = x_lim, y = c(0,1), type = "n", las = 1, xlab = "", ylab = "Vaccine coverage",
       xlim = x_lim, ylim = c(0,1), xaxt = "n" )
  draw_month_axis(start_date, end_date)
  
  for (i in 1:length(out_long_list))
  {
    data = filter_non_compartments(out_long_list[[i]]) 

    if (sum_vacs)
    {
      for (i in 1:length(vac_names))
      {
        data$vac[data$vac == vac_names[i]] = "vaccinated"
      }
      vac_names = "vaccinated"
    }
    
    data = data %>%
      group_by(time, date, vac, age) %>%
      summarise(value = sum(value), .groups = "drop") 
    
    if (by_age)
    {
      for (a in na.omit(unique(out_long_list[[1]]$age)))
      {
        data_age = filter(data, age == a)
        for (v in 1:length(vac_names))
        {
          data_age_vac = filter(data_age, vac == vac_names[v])
          lines(data_age_vac$date, data_age_vac$value / total_pop[a], lty = v, lwd = 0.5, col = alpha(cols[a], 0.5))
        }
      }
    }
    else
    {
      # sum across ages
      data = data %>%
        group_by(time, date, vac) %>%
        summarise(value = sum(value), .groups = "drop") 
      
      for (v in 1:length(vac_names))
      {
        data_vac = filter(data, vac == vac_names[v])
        lines(data_vac$date, data_vac$value / total_pop, lty = v, lwd = 0.5, col = alpha(cols, 0.5))
      }
    }
  }
  if (!is.null(title))
  {
    mtext(title, side = 3, line = 0.5)
  }
  
  if (by_age) { legend("topright", legend = c("0-19","20-49","50-69","70+"), col = cols, lty = 1, lwd = 2, bty = "n") }
  legend("topleft", legend = vac_names, lty = 1:length(vac_names), lwd = 1, bty = "n")
}

# This routine plots daily values of the chosen metric by vaccine status and (optionally) by age group.
# Added 6/29/21 - DAS
# To Do: set y_lim for non-by-age plots (for now just set it correctly outside routine)
#        (1.5 multiplier is also a bit bogus, but setting to the max did not yield any "headroom")
#
plot_metric_by_vac_status = function(out_long_list, metric = c("cases","inf", "hosp", "deaths"), 
                                     by_age = TRUE,vac_status=-1, start_date = NULL, end_date = NULL, title = NULL,x_lim=NULL,y_lim=NULL,...)
{
  if (is.null(start_date))
  {
    start_date = min(do.call(c, lapply(out_long_list, function(x) min(x$date))))
  }
  if (is.null(end_date))
  {
    end_date = max(do.call(c, lapply(out_long_list, function(x) max(x$date))))
  }
  
  state_col = get_state_col_from_metric(metric)
  label = get_label_from_metric(metric)
  
  if (is.null(x_lim)) { x_lim = c(start_date, end_date) }
  print(y_lim)
  if (is.null(y_lim))
  {
    ymax=0
    for (i in 1:length(out_long_list))
    {
      data = filter(out_long_list[[i]], state == state_col) 
      #values = apply(data$value,2,diff)
      values = data$value - lag(data$value)
      ymax = max(max(1.5*values,na.rm = TRUE),ymax)
    }
    y_lim = c(0, ymax)
    print(ymax)
  }
  
  cols = if (by_age) { c("black","darkblue","darkgreen","darkred") } else { c("black") }
  
  plot(x = x_lim, y = y_lim, type = "n", las = 1, xlab = "", ylab = paste("Daily", label,"by vaccine status"),
       xlim = x_lim, ylim = y_lim, xaxt = "n" )
  draw_month_axis(start_date, end_date)
  
  for (i in 1:length(out_long_list))
  {
    if (vac_status==0)
    {
      data = filter(out_long_list[[i]], state == state_col, vac %in% c("None","R"))
    } else if (vac_status==1)
    {
      data = filter(out_long_list[[i]], state == state_col, vac %in% c("mRNA","R_vax"))
    } else {
      data = filter(out_long_list[[i]], state == state_col)
    } 
    if (by_age)
    {
      age_data = data %>%
        group_by(time, date, age) %>%
        summarise(value = sum(value), .groups = "drop") 

      for (a in na.omit(unique(out_long_list[[1]]$age)))
      {
        data_age = filter(age_data, age == a)
        #values = apply(data_age$value,2,diff)
        values = data_age$value - lag(data_age$value)
        values = values[2:length(values)]
        dates = data_age$date[2:length(data_age$date)]
        lines(dates, values, lty = a, lwd = 0.5, col = alpha(cols[a], 0.5))
      }
    }
    else
    {
      # sum across ages
      data = data %>%
        group_by(time, date) %>%
        summarise(value = sum(value), .groups = "drop") 
      
      #values = apply(data$value,2,diff)
      values = data$value - lag(data$value)
      values = values[2:length(values)]
      dates = data$date[2:length(data$date)]
      
      lines(dates, values, lty = 1, lwd = 0.5, col = alpha(cols, 0.5))
    }
  }
  if (!is.null(title))
  {
    mtext(title, side = 3, line = 0.5)
  }
  
  if (by_age) { legend("topright", legend = c("0-19","20-49","50-69","70+"), col = cols, lty = 1, lwd = 2, bty = "n") }
  #legend("topleft", legend = vacs, lty = 1:length(vacs), lwd = 1, bty = "n")
  
}

plot_strain_proportion_current_inf = function(out_long_list, by_age = TRUE,
                                                      start_date = NULL, end_date = NULL, title = NULL)
{
  if (is.null(start_date))
  {
    start_date = min(do.call(c, lapply(out_long_list, function(x) min(x$date))))
  }
  if (is.null(end_date))
  {
    end_date = max(do.call(c, lapply(out_long_list, function(x) max(x$date))))
  }
  
  x_lim = c(start_date, end_date)
  cols = if (by_age) { c("black","darkblue","darkgreen","darkred") } else { c("black") }
  
  plot(x = x_lim, y = c(0,1), type = "n", las = 1, xlab = "", ylab = "Strain proportion current infections",
       xlim = x_lim, ylim = c(0,1) ) #, xaxt = "n" )
  #draw_month_axis(start_date, end_date)
  
  for (i in 1:length(out_long_list))
  {
    data = filter(out_long_list[[i]], state == "E") 
    data = data %>%
      group_by(time, date, strain, age) %>%
      summarise(value = sum(value), .groups = "drop") 
    strains = unique(data$strain)
    
    if (by_age)
    {
      data_agg = data %>%
        group_by(time, date, age) %>%
        summarise(value = sum(value), .groups = "drop") 
      
      for (a in na.omit(unique(out_long_list[[1]]$age)))
      {
        data_age = filter(data, age == a)
        data_age_agg = filter(data_agg, age == a)
        
        for (s in 1:length(strains))
        {
          data_age_str = filter(data_age, strain == strains[s])
          lines(data_age_str$date, data_age_str$value / data_age_agg$value, lty = s, lwd = 0.5, col = alpha(cols[a], 0.5))
        }
      }
    }
    else
    {
      # sum across ages
      data = data %>%
        group_by(time, date, strain) %>%
        summarise(value = sum(value), .groups = "drop") 
      data_agg = data %>%
        group_by(time, date) %>%
        summarise(value = sum(value), .groups = "drop") 
      
      for (s in 1:length(strains))
      {
        data_str = filter(data, strain == strains[s])
        lines(data_str$date, data_str$value / data_agg$value, lty = s, lwd = 0.5, col = alpha(cols, 0.5))
      }
    }
  }
  if (!is.null(title))
  {
    mtext(title, side = 3, line = 0.5)
  }
  
  if (by_age) { legend("topright", legend = c("0-19","20-49","50-69","70+"), col = cols, lty = 1, lwd = 2, bty = "n") }
  legend("topleft", legend = strains, lty = 1:length(strains), lwd = 1, bty = "n")
  
}

plot_strain_proportion_median = function(out_long_list, start_date = NULL, end_date = NULL, title = NULL)
{
  if (is.null(start_date))
  {
    start_date = min(do.call(c, lapply(out_long_list, function(x) min(x$date))))
  }
  if (is.null(end_date))
  {
    end_date = max(do.call(c, lapply(out_long_list, function(x) max(x$date))))
  }
  
  x_lim = c(start_date, end_date)
  
  plot(x = x_lim, y = c(0,1), type = "n", las = 1, xlab = "", ylab = "Strain proportion",
       xlim = x_lim, ylim = c(0,1) ) #, xaxt = "n" )
  #draw_month_axis(start_date, end_date)
  
  # store the results to calculate the median
  strains = unique(filter(out_long_list[[1]], state == "E")$strain)
  results = list()
  for (i in 1:length(strains))
  {
    results[[i]] = data.frame(date = sort(unique(out_long_list[[1]]$date)))
  }
  
  for (i in 1:length(out_long_list))
  {
    data = filter(out_long_list[[i]], state == "E") 
    data = data %>%
      group_by(time, date, strain, age) %>%
      summarise(value = sum(value), .groups = "drop") 
    
    # sum across ages
    data = data %>%
      group_by(time, date, strain) %>%
      summarise(value = sum(value), .groups = "drop") 
    data_agg = data %>%
      group_by(time, date) %>%
      summarise(value = sum(value), .groups = "drop") 
    
    for (s in 1:length(strains))
    {
      data_str = filter(data, strain == strains[s]) %>% select(date, value)
      data_str$value = data_str$value / data_agg$value
      results[[s]] = left_join(results[[s]], data_str, by = "date", suffix = c("", i))
    }
  }
  
  # calculate median
  
  for (s in 1:length(strains))
  {
    strain_per = apply(select(results[[s]], !date), 1, median)
    lines(results[[s]]$date, strain_per, lty = s, lwd = 3)
  }
  
  
  if (!is.null(title))
  {
    mtext(title, side = 3, line = 0.5)
  }
  
  legend("right", legend = strains, lty = 1:length(strains), lwd = 3, bty = "n")
  
}

plot_strain_proportion_date = function(out_long_list, plot_date, title = NULL)
{
  results = NULL

  for (i in 1:length(out_long_list))
  {
    data = filter(out_long_list[[i]], state == "E" & date == plot_date)  %>%
      group_by(time, date, strain) %>%
      summarise(value = sum(value), .groups = "drop") 
    data$value = data$value / sum(data$value) # convert to percent
    
    results = rbind(results, data)
  }
  strains = unique(results$strain)
  
  
  ggplot(results, aes(value, fill = strain)) + geom_density(alpha = 0.2) +
    xlim(c(0, 1)) + xlab("prevalence") + ylab(paste("density on", plot_date)) +
    ggtitle(title) +
    theme_classic()
}

plot_projection_from_data_set = function(data_set, metric = c("cases", "inf", "hosp", "deaths", "negtests"), title = NULL, start_date = NULL, end_date = NULL,
                                         y_lim = NULL)
{
  if (is.null(start_date))
  {
    start_date = min(do.call(c, lapply(data_set, function(x) min(x$date))))
  }
  if (is.null(end_date))
  {
    end_date = max(do.call(c, lapply(data_set, function(x) max(x$date))))
  }
  
  # set columns and labels
  model_col = get_model_col_from_metric(metric, by_age = FALSE)
  label = get_label_from_metric(metric)
  
  x_lim = c(start_date, end_date)
  if (is.null(y_lim))
  {
      y_lim = c(0, max(do.call(c, lapply(data_set, function(x) max(x[,model_col], na.rm = TRUE)))))
  }
  
  plot(x = x_lim, y = y_lim, type = "n", las = 1, xlab = "", ylab = paste("Daily", label),
       xlim = x_lim, ylim = y_lim, xaxt = "n" )
  draw_month_axis(start_date, end_date)
  
  for (i in 1:length(data_set))
  {
    lines(x = data_set[[i]]$date, y = data_set[[i]][,model_col, drop = TRUE], lty = 1, lwd = 0.5, col = alpha("black", 0.25))
  }
  if (!is.null(title))
  {
    mtext(title, side = 3, line = 0.5)
  }
}

get_state_col_from_metric = function(metric = c("cases","inf", "hosp", "deaths"))
{
  metric = match.arg(metric)

  col = switch(metric,
             "cases" = "cum_diag",
             "inf" = "cum_exp",
             "hosp" = "cum_hosp",
             "deaths" = "cum_death",
             stop("Bad metric argument") )
 
  return(col)
}
get_model_col_from_metric = function(metric = c("cases", "inf", "hosp", "deaths", "negtests"), by_age = TRUE)
{
  metric = match.arg(metric)
  prefix = if (by_age) { "" } else { "tot_" }
  suffix = if (by_age) { paste0("-", 1:4) } else { "" }

  col = switch(metric,
             "cases" = paste0(prefix, "diag", suffix),
             "inf" = paste0(prefix, "inf", suffix),
             "hosp" = paste0(prefix, "hosp", suffix),
             "deaths" = paste0(prefix, "death", suffix),
             "negtests" = paste0(prefix, "testneg", suffix),
             stop("Bad metric argument") )
 
  return(col)
}

get_data_col_from_metric = function(metric = c("cases", "inf", "hosp", "deaths", "negtests"), by_age = TRUE)
{
  metric = match.arg(metric)
  suffix = if (by_age) { 1:4 } else { "" }
  
  col = switch(metric,
               "cases" = if (by_age) { "pos" } else { "daily.pos" },
               "inf" = if (by_age) { "pos" } else { "daily.pos" },
               "hosp" = if (by_age) { "hosps" } else { "Hospitalizations" },
               "deaths" = "deaths" ,
               "negtests" = if (by_age) { "neg" } else { stop("No neg test column across ages") } )
  col = paste0(col, suffix)
  return(col)
}

get_label_from_metric = function(metric = c("cases", "inf", "hosp", "deaths", "negtests"))
{
  metric = match.arg(metric)
  label = switch(metric,
                 "cases" = "Cases",
                 "inf" = "Infections (model)",
                 "hosp" = "Hospitalizations",
                 "deaths" = "Deaths",
                 "negtests" = "Negative Tests",
                 stop("Bad metric argument") )

  return(label)
}

calc_y_lim_from_scenario_list = function(scenario_list, metric = c("cases", "inf", "hosp", "deaths", "negtests"), by_age = TRUE)
{
  model_col = get_model_col_from_metric(metric, by_age)
  max_val = max(sapply(scenario_list, function(s) max(do.call(c, lapply(s, function(x) max(x[,model_col], na.rm = TRUE))))))
  y_lim = c(0, max_val)
  return(y_lim)
}

plot_param_posteriors = function(param_matrix, param_names, param_priors, reference_param_set = NULL)
{
  for (i in 1:length(param_names))
  {
    lim = if (is.list(param_priors[[i]])) { range(param_matrix[,i]) } else { as.numeric(param_priors[[i]][2:3]) }
    hist(param_matrix[,i], xlab = param_names[i], main = "", xlim = lim)
    if (!is.null(reference_param_set))
    {
      abline(v = reference_param_set[i], col = "blue")
    }
  }
}

plot_total_infected = function(param_data_list)
{
  hist(sapply(param_data_list, function(x) max(x[,"cum_tot_inf"], na.rm = TRUE)) / kc_pop * 100, xlab = paste("Percent infected as of", max(param_data_list[[1]]$date)) , main = "")
}

# out_matrix is the output of run_model()
plot_r_eff = function(out_matrix, params_base)
{
  r_eff = out_matrix[,which(startsWith(colnames(out_matrix), "r_eff"))]
  plot(params_base$model_day0_date + out_matrix[,1], r_eff, ylim = c(0, max(r_eff)),  xlab = "", ylab = "R efective", type = "l")
  abline(h = 1, lty = 2, col = "gray80")
}

# param_matrix is matrix where each row is a parameterization (i.e. from calibration)
plot_r_eff_from_param_set = function(param_matrix, params_names, params_base, params_temporal, init_state, start_date = NULL, end_date = NULL)
{
  model_out = get_model_data_param_sets(param_matrix, params_names, params_base, params_temporal, 
                                        init_state, NULL, start_date, end_date, calc_r_eff = TRUE, out_type = "matrix")
  return(plot_r_eff_from_model_list(model_out, param_matrix, params_names, params_base))
}

# param_matrix as above, and model_list is a list of the matrix model output from run_model() for the param_matrix with R eff already calculated
plot_r_eff_from_model_list = function(model_list, param_matrix, params_names, params_base)
{
  n = length(model_list)
  ns_out = sapply(model_list, nrow)
  start_day = sapply(model_list, function(x) min(x[,1]))
  start_date = get_date_from_model_day(start_day, params_base$model_day0_date)
  r_eff_col = which(startsWith(colnames(model_list[[1]]), "r_eff"))
  
  x_lim = min(start_date) + c(0, max(ns_out))
  y_lim = c(0, max(sapply(model_list, function(x) max(x[,r_eff_col]))))

  plot(x_lim, y_lim, type = "n", xlim = x_lim, ylim = y_lim, xlab = "", ylab = "R efective")

  for (i in 1:n)
  {
    xs = seq(start_date[i], length = ns_out[i], by = 1) # specifying end_date would just go to the day before
    lines(xs, model_list[[i]][,r_eff_col], lwd = 0.5, col = alpha("black", 0.5))
  }
  abline(h = 1, lty = 2, col = "gray80")
}

# this is to plot R eff that is aleady calculated from output matrix and is now a state in the long output
plot_r_eff_from_long_list = function(long_list, y_lim = NULL, title = NULL, start_date = NULL, end_date = NULL)
{
  if (is.null(start_date))
  {
    start_date = min(do.call(c, lapply(long_list, function(x) min(x$date))))
  }
  if (is.null(end_date))
  {
    end_date = max(do.call(c, lapply(long_list, function(x) max(x$date))))
  }
  
  x_lim = c(start_date, end_date)
  if (is.null(y_lim))
  {
    y_lim = c(0, max(do.call(c, lapply(long_list, function(x) max(filter(x, state == "r_eff")$value, na.rm = TRUE)))))
  }
  
  plot(x = x_lim, y = y_lim, type = "n", las = 1, xlab = "", ylab = "R effective",
       xlim = x_lim, ylim = y_lim )
  
  for (i in 1:length(long_list))
  {
    data = filter(long_list[[i]], state == "r_eff")
    lines(x = data$date, y = data$value, lty = 1, lwd = 0.5, col = alpha("black", 0.5))
  }
  
  abline(h = 1, lty = 2, col = "black")
}

# R0 estimated from bstar
plot_r0_from_param_set = function(param_matrix, param_names, params_base)
{
  # this is using the old estimation for bounds for bstar, plus adjusting for contact matrix differences between V2 and V1
  # bstar = R0 / (beta_p / gamma_2 + id)
  # => R0 = bstar * (beta_p / gamma_2 + id) * sum(C_V2) / sum(C_V1)
  r0s = param_matrix[, which(param_names == "bstar")] * (params_base$beta_p / params_base$gamma_2 + params_base$id) * sum(params_base$C) / 4
  hist(r0s, main = "", xlab = "R0 estimated from bstar")
  
}
  
plot_param_sets = function(doy, result_matrix, doy_actual, result_actual, y_lab = "result",
                           col_pal = "black", col_idx = 1, x_lim = NULL, y_lim = NULL, ...)
{
  data_end = min(which(apply(result_matrix, 1, function(x) all(is.na(x))) & doy > 50))
  if (is.infinite(data_end)) { data_end = nrow(result_matrix) }
  if (is.null(x_lim)) { x_lim = c(1, data_end) }
  if (is.null(y_lim)) { y_lim = c(0, max(result_matrix, na.rm = TRUE)) }

  plot(x = 0, y = 0, type = "n", xlab = "date", ylab = y_lab,
       xlim = x_lim, ylim = y_lim, xaxt = "n", ...)
  draw_month_axis() 
  
  for (i in 1:ncol(result_matrix))
    lines(x = doy, y = result_matrix[,i], lty = 1, lwd = 0.75, col = col_pal[col_idx[i]])
  lines(x = doy_actual, y = result_actual, col = "red", lwd = 3)
  
}


plot_percent_hosp = function(out_data, cols, ..., legend = TRUE)
{
  plot(x = 0, y = 0, type = "n", xlim = range(out_data[,"time"]), ylim = c(0, 0.5),
       xlab = "model time", ylab = "percent symp hospitalized", bty = "l", ...)

  for (age in 1:4)
  {
    lines(out_data[,"time"], out_data[,paste0("cum_hosp_i", age)] / out_data[,paste0("cum_sym_i", age)], 
          col = cols[age])
  }
  lines(out_data[,"time"], sum_across_ages(out_data, col_start = "cum_hosp_i")$all / sum_across_ages(out_data, col_start = "cum_sym_i")$all, 
      col = "black", lwd = 1.5)
  
  if (legend)
  {
    legend("topleft", legend = c(paste("age", 1:4), "all"), col = c(cols, "black"), lty = 1, bty = "n")
  }
}

draw_month_axis = function(start_date, end_date, by = 1)
{
  labels = seq(from= start_date, to = end_date, by = paste(by, "month"))
  axis(side = 1, at = labels, labels = format(labels, "%b")) 
}
