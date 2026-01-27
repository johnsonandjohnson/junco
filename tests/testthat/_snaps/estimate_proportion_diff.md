# s_proportion_diff_j works as expected

    list(diff = structure(c(diff_ha = 2.00080032012805), label = "Difference in Response rate (%)"), 
        diff_ci = structure(c(diff_ci_ha_l = -15.6352689794718, diff_ci_ha_u = 19.6368696197279
        ), label = "90% CI (Anderson-Hauck)"), diff_est_ci = structure(c(diff_ha = 2.00080032012805, 
        diff_ci_ha_l = -15.6352689794718, diff_ci_ha_u = 19.6368696197279
        ), label = "% Difference (90% CI)"), diff_ci_3d = structure(c(diff_ha = 2.00080032012805, 
        diff_ci_ha_l = -15.6352689794718, diff_ci_ha_u = 19.6368696197279
        ), label = "Relative Risk (90% CI)"))

---

    list(diff = structure(c(diff_cmh = 2.88879328887933), label = "Difference in Response rate (%)"), 
        diff_ci = structure(c(diff_ci_cmh_l = -13.1726676720747, 
        diff_ci_cmh_u = 18.9502542498333), label = "90% CI (CMH, without correction)"), 
        diff_est_ci = structure(c(diff_cmh = 2.88879328887933, diff_ci_cmh_l = -13.1726676720747, 
        diff_ci_cmh_u = 18.9502542498333), label = "% Difference (90% CI)"), 
        diff_ci_3d = structure(c(diff_cmh = 2.88879328887933, diff_ci_cmh_l = -13.1726676720747, 
        diff_ci_cmh_u = 18.9502542498333), label = "Relative Risk (90% CI)"))

# a_proportion_diff_j works as expected in a table layout

    Code
      result
    Output
                                      A           B
      —————————————————————————————————————————————
      % Difference (90% CI)   2.0 (-15.6, 19.6)    

