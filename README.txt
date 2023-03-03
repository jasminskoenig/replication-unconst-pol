
REPLICATION FILES FOR:

    Do Populist Parties In Government Produce Unconstitutional Policies?
    Evidence from Austria, 1980-2021
    Jasmin König and Tilko Swalve

    Please contact Jasmin König or Tilko Swalve should you encounter any problems
    or have questions regarding these replication files. Current contact details are
    available at: www.jasminskoenig.com or www.tilko-swalve.de

    October 22, 2022


OVERVIEW
---------------

The replication files consist of

- 21 .R files
- 2 .rds data files
- 2 .xlsx data files
- 1 .txt readme file

All tables (with the exception of table 1) and figures (with the exception of figure 1 and 12) in the paper were generated using R. 

To replicate figures 2-5 in the main text, run main_all_figures.R. 
To replicate tables 2-3 in the main text, run main_all_tables.R. 
To replicate figures 6-11 in the appendix, run app_all_figures.R.
To replicate tables 4-8 in the appendix, run app_all_tables.R.



CORRESPONDENCE
---------------

Main text
- Figure 2: fig_totalcomplaints.pdf
- Figure 3: fig_lawsovertime.pdf
- Figure 4: fig_lawsreviewd_bypop.pdf
- Figure 5: fig_lawsreviewd_byplaintiff.pdf

Appendix
- Figure 6: fig_vdem.pdf
- Figure 7: fig_trust.pdf
- Figure 8: fig_vparty.pdf
- Figure 9: fig_review1.pdf
- Figure 10: fig_review2.pdf
- Figure 11: fig_review3.pdf

Main text
- Table 2: tab_regressionresults.tex
- Table 3: tab_suedlaws.tex

Appendix
- Table 4: tab_rob_alldecisions.tex
- Table 5: tab_rob_popdum.tex
- Table 6: tab_rob_popdumall.tex
- Table 7: tab_rob_fixedeffects.tex
- Table 8: tab_rob_2000-21.tex


R sessionInfo()
---------------

R version 4.1.1 (2021-08-10)Platform: aarch64-apple-darwin20 (64-bit)Running under: macOS Big Sur 11.6Matrix products: defaultLAPACK: /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/lib/libRlapack.dyliblocale:[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8attached base packages:[1] stats     graphics  grDevices utils     datasets  methods   base     other attached packages: [1] readxl_1.3.1        vdemdata_2.0        hrbrthemes_0.8.6    modelsummary_1.0.2  lubridate_1.8.0     [6] data.table_1.14.2   fixest_0.10.4       optimx_2022-4.30    sjstats_0.18.1      lme4_1.1-27.1      [11] Matrix_1.3-4        stargazer_5.2.2     broom.mixed_0.2.9.4 forcats_0.5.1       stringr_1.4.0      [16] dplyr_1.0.8         purrr_0.3.4         readr_2.1.2         tidyr_1.1.4         tibble_3.1.6       [21] ggplot2_3.3.5       tidyverse_1.3.1     rstudioapi_0.14