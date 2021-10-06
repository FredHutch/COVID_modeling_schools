# Make the box plots for the school reopening / vaccinate kids scenarios


cols = c("#7fbf7b", "#67a9cf", "#af8dc3") # for vaccinate kids 10/1 and 1/1, no kids, 
cols_light = c("#d9f0d3", "#d1e5f0", "#e7d4e8") # green, blue, purple

# helper functions

do_box_plot_12 = function(data, ylab, column = NULL, title = "", ylim = NULL)
{
  if (!is.null(column)) 
  {
    # rename that one to value
    names(data)[which(names(data) == "value")] = "old_value"
    names(data)[which(names(data) == column)] = "value"
  }
  if (is.null(ylim))
  {
    ylim = range(data[,"value"])
  }
  
  
  plot(0, 0, type = "n", xaxt = "n",
       xlim = c(0.5, 13.5), ylim = ylim,
       xlab = "", ylab = ylab, main = title)
  boxplot(value ~ scenario_school, data = data, subset = scenario_vac == "October",
          at = 1:4, add = TRUE,yaxt = "n",
          col = cols[1], cex.axis=0.8)
  boxplot(value ~ scenario_school, data = data, subset = scenario_vac == "January",
          at = 5.5:8.5, add = TRUE, yaxt = "n",
          col = cols[2], cex.axis=0.8)
  boxplot(value ~ scenario_school, data = data, subset = scenario_vac == "No",
          at = 10:13, add = TRUE, yaxt = "n",
          col = cols[3], cex.axis=0.8)
}

do_box_plot_8 = function(data, ylab, column = NULL, title = "", ylim = NULL)
{
  if (!is.null(column)) 
  {
    # rename that one to value
    names(data)[which(names(data) == "value")] = "old_value"
    names(data)[which(names(data) == column)] = "value"
  }
  if (is.null(ylim))
  {
    ylim = range(data[,"value"])
  }
  
  plot(0, 0, type = "n", xaxt = "n",
       xlim = c(0.5, 9), ylim = ylim,
       xlab = "", ylab = ylab, main = title)
  abline(h = 0, col = "gray50")
  boxplot(value ~ scenario_school, data = data, subset = scenario_vac == "October",
          at = 1:4, add = TRUE, yaxt = "n",
          col = cols[1], cex.axis=0.8)
  boxplot(value ~ scenario_school, data = data, subset = scenario_vac == "January",
          at = 5.5:8.5, add = TRUE, yaxt = "n",
          col = cols[2], cex.axis=0.8)
}

make_factors = function(consolidated_scenarios)
{
  if (!("scenario_school" %in% names(consolidated_scenarios)))
  {
    scenario_parts = plyr::ldply(strsplit(consolidated_scenarios$scenario_name, '\n'))
    names(scenario_parts) = c("scenario_school", "scenario_vac")
    consolidated_scenarios = cbind(consolidated_scenarios, scenario_parts)
  }

  if ("vac" %in% names(consolidated_scenarios))
  {
    consolidated_scenarios$vac = recode(consolidated_scenarios$vac,  None = "unvaccinated", Recovered = "unvaccinated", 
                                        Vax = "vaccinated",  RecVax = "vaccinated", SuscVax = "vaccinated")
  }
  consolidated_scenarios$scenario_name = factor(consolidated_scenarios$scenario_name, levels = unique(consolidated_scenarios$scenario_name), ordered = TRUE) # keep original order
  consolidated_scenarios$scenario_school = factor(consolidated_scenarios$scenario_school, levels = unique(consolidated_scenarios$scenario_school), ordered = TRUE) 
  
  return(consolidated_scenarios)
}

get_totals_all = function(final_cumul)
{
  final_cumul %>% group_by(sim, scenario_name, scenario_school, scenario_vac) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") 
}

get_totals_by_age = function(final_cumul)
{
  final_cumul_by_age = final_cumul %>% group_by(sim, scenario_name, scenario_school, scenario_vac, age) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") 
  final_cumul_by_age$scenario_name_age = paste0(final_cumul_by_age$scenario_name, "\nage ", final_cumul_by_age$age)
  final_cumul_by_age$scenario_name_age = factor(final_cumul_by_age$scenario_name_age, levels = unique(final_cumul_by_age$scenario_name_age), ordered = TRUE)
  return(final_cumul_by_age)
}

get_totals_by_vac = function(final_cumul)
{
  final_cumul_by_vac = final_cumul %>% group_by(sim, scenario_name, scenario_school, scenario_vac, vac) %>%
    summarise(value = sum(value, na.rm = TRUE), total = sum(total, na.rm = TRUE), .groups = "drop") 
  # note that scenario_vac is not included so it must be subset!
  final_cumul_by_vac$scenario_name_vac = paste0(as.character(final_cumul_by_vac$scenario_school), "\n", final_cumul_by_vac$vac)
  final_cumul_by_vac$scenario_name_vac = factor(final_cumul_by_vac$scenario_name_vac, levels = unique(final_cumul_by_vac$scenario_name_vac), ordered = TRUE)
  
  # vaccine do per 100K for final totals
  final_cumul_by_vac = final_cumul_by_vac %>% mutate(per_capita = value / total * 100000)
  
  return(final_cumul_by_vac)
}

do_box_plots = function(final_cumul, days_at_max_sd, max_daily, metric_name)
{
  # box plot for scenarios summed across age and vaccination status
  final_cumul_totals = get_totals_all(final_cumul)
  
  do_box_plot_12(final_cumul_totals, paste("Cumulative", metric_name), title = "Overall")
  
  # now do number and percentage averted
  final_cumul_totals = left_join(final_cumul_totals, 
                                 final_cumul_totals %>% filter(scenario_vac == "No") %>% dplyr::select(sim, scenario_school, value),
                                 by = c("sim", "scenario_school"), suffix = c("", "_ref"))
  
  # number
  final_cumul_totals = final_cumul_totals %>% mutate(num_avert = (value_ref - value))
  do_box_plot_8(final_cumul_totals, paste(metric_name, "averted"), column = "num_avert", title = "Overall")
  
  
  # now calculate percentage
  final_cumul_totals = final_cumul_totals %>% mutate(per_avert = num_avert / value_ref * 100)
  do_box_plot_8(final_cumul_totals, paste("Percentage", metric_name, "averted"), column = "per_avert", title = "Overall", ylim = c(-25, 100))
  
  # and for max sd
  days_at_max_sd = left_join(days_at_max_sd, 
                             days_at_max_sd %>% filter(scenario_vac == "No") %>% dplyr::select(sim, scenario_school, value),
                             by = c("sim", "scenario_school"), suffix = c("", "_ref"))
  days_at_max_sd = days_at_max_sd %>% mutate(time_avert = (value_ref - value))
  
  
  # scatterplot of number hospitalizations averted vs time at max sd
  oldpar = par()
  par(mfrow = c(1,4), mar = c(4, 4, 2, 0), mgp = c(2, 0.5, 0))
  scens_school = unique(final_cumul_totals$scenario_school)
  scens_vac = unique(final_cumul_totals$scenario_vac)
  xlim = range(final_cumul_totals$num_avert)
  ylim = range(days_at_max_sd$time_avert)
  
  # need to join because data frames are not in same order
  final_cumul_totals$sim = as.numeric(final_cumul_totals$sim)
  final_cumul_totals = left_join(final_cumul_totals, days_at_max_sd, by = c("sim", "scenario_name", "scenario_school", "scenario_vac"), suffix = c("", ".max_sd"))
  
  for (i in 1:length(scens_school))
  {
    totals_subset = filter(final_cumul_totals, scenario_school == scens_school[i] & scenario_vac != "No")
    plot(totals_subset$num_avert, jitter(totals_subset$time_avert, amount = 0.5), col = alpha(cols[match(totals_subset$scenario_vac, scens_vac)], 0.5), pch = 16,
         xlab = paste("Difference cumulative", metric_name), ylab = "Difference percentage time at maximum SD", main = scens_school[i],
         xlim = xlim, ylim = ylim)
  }
  par(oldpar)
  
  
  # box plot for scenarios for age groups
  cols_purple = rev(brewer.pal(9, "PRGn")[1:4])
  cols_green = brewer.pal(9, "PRGn")[6:9]
  cols_blue = brewer.pal(9, "RdBu")[6:9]
  
  final_cumul_by_age = get_totals_by_age(final_cumul)
    
  # and percentage averted
  final_cumul_by_age = left_join(final_cumul_by_age, 
                                 final_cumul_by_age %>% filter(scenario_vac == "No") %>% dplyr::select(sim, scenario_school, age, value),
                                 by = c("sim", "scenario_school", "age"), suffix = c("", "_ref"))
  final_cumul_by_age = final_cumul_by_age %>% mutate(num_avert = (value_ref - value))
  final_cumul_by_age = final_cumul_by_age %>% mutate(per_avert = num_avert / value_ref * 100)
  
  boxplot(final_cumul_by_age$value ~ final_cumul_by_age$scenario_name_age, 
          xlab = "", ylab = paste("Cumulative", metric_name), xaxt = "n", xlim = c(1, 48),
          col = c(rep(cols_green, 4), rep(cols_blue, 4), rep(cols_purple, 4)))
  abline(v = 4.5 + 4 * (0:10), col = "gray80", lwd = 0.5)
  # abline(v = c(16.5, 32.5), col = "gray50", lwd = 0.75)
  axis(side = 1, line = -1, at = 2.5 + 4 * (0:11), labels = rep(unique(final_cumul_by_age$scenario_school), 3), tick = FALSE, cex.axis = 0.8)
  axis(side = 1, line = 0, at = c(8.5, 24.5, 40.5), labels = c("October vaccination", "January vaccination", "No vaccination"), tick = FALSE, cex.axis = 0.8)
  legend("topright", inset = c(-0.15, 0), xpd = TRUE, bty = "n",
         legend = c("Age 0-19", "Age 20-49", "Age 50-69", "Age 70+"), fill = c("gray90", "gray70", "gray50", "gray30"))
  
  # now for each age separately
  
  do_box_plot_12(subset(final_cumul_by_age, age == 1), paste("Cumulative", metric_name), title = "Age 0-19 years")
  do_box_plot_8(subset(final_cumul_by_age, age == 1), paste(metric_name,  "averted"), column = "num_avert", title = "Age 0-19 years")
  do_box_plot_8(subset(final_cumul_by_age, age == 1), paste("Percentage", metric_name,  "averted"), column = "per_avert", title = "Age 0-19 years", ylim=c(-25,100))
  do_box_plot_12(subset(final_cumul_by_age, age == 2), paste("Cumulative", metric_name), title = "Age 20-49 years")
  do_box_plot_8(subset(final_cumul_by_age, age == 2), paste(metric_name,  "averted"), column = "num_avert", title = "Age 20-49 years")
  do_box_plot_8(subset(final_cumul_by_age, age == 2), paste("Percentage", metric_name,  "averted"), column = "per_avert", title = "Age 20-49 years", ylim=c(-25,100))
  do_box_plot_12(subset(final_cumul_by_age, age == 3), paste("Cumulative", metric_name), title = "Age 50-69 years")
  do_box_plot_8(subset(final_cumul_by_age, age == 3), paste(metric_name,  "averted"), column = "num_avert", title = "Age 50-69 years")
  do_box_plot_8(subset(final_cumul_by_age, age == 3), paste("Percentage", metric_name,  "averted"), column = "per_avert", title = "Age 50-69 years", ylim=c(-25,100))
  do_box_plot_12(subset(final_cumul_by_age, age == 4), paste("Cumulative", metric_name), title = "Age 70+ years")
  do_box_plot_8(subset(final_cumul_by_age, age == 4), paste(metric_name,  "averted"), column = "num_avert", title = "Age 70+ years")
  do_box_plot_8(subset(final_cumul_by_age, age == 4), paste("Percentage", metric_name,  "averted"), column = "per_avert", title = "Age 70+ years", ylim=c(-25,100))
  
  
  # box plot for scenarios for vaccination status
  final_cumul_by_vac = get_totals_by_vac(final_cumul)
  
  boxplot(value ~ scenario_name_vac, data = final_cumul_by_vac, subset = scenario_vac == "October",
          at = 1:8, xlim = c(0.5, 25.5), ylim = range(final_cumul_by_vac$value), xaxt = "n",
          xlab = "", ylab = paste("Cumulative", metric_name),
          col = c(cols_light[1], cols[1]))
  boxplot(value ~ scenario_name_vac, data = final_cumul_by_vac, subset = scenario_vac == "January",
          at = 9.5:16.5, add = TRUE, xaxt = "n",
          col = c(cols_light[2], cols[2]))
  boxplot(value ~ scenario_name_vac, data = final_cumul_by_vac, subset = scenario_vac == "No",
          at = 18:25, add = TRUE, xaxt = "n",
          col = c(cols_light[3], cols[3]))
  axis(side = 1, line = -1, at = c(1.5 + 2 * (0:3), 2 + 2 * (4:7), 2.5 + 2 * (8:11)), labels = rep(unique(final_cumul_by_age$scenario_school), 3), tick = FALSE, cex.axis = 0.8)
  axis(side = 1, line = 0, at = c(4.5, 12.5, 20.5), labels = c("October vaccination", "January vaccination", "No vaccination"), tick = FALSE, cex.axis = 0.8)
  legend("topright", inset = c(-0.17, 0), xpd = TRUE, bty = "n",
         legend = c("unvaccinated", "vaccinated"), fill = c("gray80", "gray50"))
  
  
  boxplot(per_capita ~ scenario_name_vac, data = final_cumul_by_vac, subset = scenario_vac == "October",
          at = 1:8, xlim = c(0.5, 25.5), ylim = range(final_cumul_by_vac$per_capita), xaxt = "n",
          xlab = "", ylab = paste("Cumulative", metric_name, "per 100K"),
          col = c(cols_light[1], cols[1]))
  boxplot(per_capita ~ scenario_name_vac, data = final_cumul_by_vac, subset = scenario_vac == "January",
          at = 9.5:16.5, add = TRUE, xaxt = "n",
          col = c(cols_light[2], cols[2]))
  boxplot(per_capita ~ scenario_name_vac, data = final_cumul_by_vac, subset = scenario_vac == "No",
          at = 18:25, add = TRUE, xaxt = "n",
          col = c(cols_light[3], cols[3]))
  axis(side = 1, line = -1, at = c(1.5 + 2 * (0:3), 2 + 2 * (4:7), 2.5 + 2 * (8:11)), labels = rep(unique(final_cumul_by_age$scenario_school), 3), tick = FALSE, cex.axis = 0.8)
  axis(side = 1, line = 0, at = c(4.5, 12.5, 20.5), labels = c("October vaccination", "January vaccination", "No vaccination"), tick = FALSE, cex.axis = 0.8)
  legend("topright", inset = c(-0.17, 0), xpd = TRUE, bty = "n",
         legend = c("unvaccinated", "vaccinated"), fill = c("gray80", "gray50"))
  
  # box plot for max sd
  do_box_plot_12(days_at_max_sd, "Percentage time at maximum SD")
  
  n = length(unique(days_at_max_sd$scenario_name))
  zeros <- vector(length=n)
  for (i in 1:n) {
    zeros[i] = signif(sum(days_at_max_sd$value[(1+100*(i-1)):(100*i)]==0),2)
  }
  print("Percentage of parameter sets with no days at SD max (by scenario)")
  print(zeros)
  
  print(unique(days_at_max_sd$scenario_school))
  
  barplot(zeros,main ="Percentage of Parameter Sets w/ No SD Tightening",
          xlab = "", ylab = "Percentage of Sets",beside=TRUE, xaxt = "n",xlim = c(0.5, 15), ylim=c(0,100),
          col = c(rep(cols[1],4),rep(cols[2],4),rep(cols[3],4)), cex.axis=0.8)
  #axis(side = 1, line = -1, at = c(0.5 + 1.2*(0:3), 1.5 + 1.2*(4:7), 2.5 + 1.2*(8:11)), labels = rep(unique(days_at_max_sd$scenario_school), 3), tick = FALSE, cex.axis = 0.6)
  axis(side = 1, line = -1, at = c(0.5 + 1.2*(0:11)), labels = rep(unique(days_at_max_sd$scenario_school), 3), tick = FALSE, cex.axis = 0.8)
  axis(side = 1, line = 0, at = c(2.5, 6.5, 11.5), labels = c("October vaccination", "January vaccination", "No vaccination"), tick = FALSE, cex.axis = 0.8)
  
  # box plot for max daily hosp
  max_daily$scenario_name = factor(max_daily$scenario_name, levels = unique(max_daily$scenario_name), ordered = TRUE) # keep original order
  max_daily$scenario_school = factor(max_daily$scenario_school, levels = unique(max_daily$scenario_school), ordered = TRUE) 
  
  do_box_plot_12(max_daily, paste("Peak", metric_name), title = "Overall")
  do_box_plot_12(max_daily, paste("Peak", metric_name), column = "children", title = "Children age 0-19")
  do_box_plot_12(max_daily, paste("Peak", metric_name), column = "adult", title = "Adults age 20+")
}

