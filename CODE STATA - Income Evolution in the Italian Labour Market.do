
// EMPIRICAL RESEARCH METHODS AND DATA ANALYSIS
// GROUP 20
// STUDY ON INCOME IN THE ITALIAN LABOR MARKET
// BISCIONE ELEONORA
// CAMPO ORAZIO
// COIANIZ IRENE
// D'ERRICO ANTONIO MARCO



////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////

// SETTING UP

// setting the directory 

cd "C:\Users\ac18a\Videos\STATA PROJECTS\STATA PROJECT"

// generating the log file

log using "LOG_RESEARCH_ON_INCOME.smcl", replace

// Using a 2% extract of INPS Data on Italian Workers from 2005 to 2016.

use alldata.dta

drop mese anno_nasc anno_morte id_azienda_madre posizione ateco07_2_calc regioncode COD_REG _spell jobloss 

ssc install estout, replace



////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////

// GENERATING OUR VARIABLES

// Variables added: n_lavori, timemax, primo_stipendio, reduced_class_dim, income_nuovo_lavoro, ultimo_stipendio, RGDP_adj_income_nuovo_lavoro, RGDP_adj_ultimo_stipendio.

// Firstly, we only want to consider income from employment

replace income = . if emp == 0

// 1) N_LAVORI

// The first variable we are interested in gnerating is the number of jobs that a person changes throughout the our range of time, so that we can use it to see wheter it is better to chage job often or remaining loyal to the same firm in order to have an increase in salary.

egen n_lavori = count(id_lavoratore) if id_azienda[_n] != id_azienda[_n - 1] | empduration == 1, by(id_lavoratore)

gen cambio_lavoro = 1 if n_lavori != .

by id_lavoratore: replace n_lavori = n_lavori[1]

drop id_azienda empduration

// 2) TIMEMAX

// We create the variable timemax that shows how much time a person remains in the dataset  

egen timemax = max(time), by(id_lavoratore)

// 3) PRIMO_STIPENDIO

// We created this variable to show what is the first income when a person enters the dataset

by id_lavoratore: gen primo_stipendio = income if time==1

// 4) REDUCED_CLASS_DIM with descriptive stat

// We group firm size to reduce the quantity of dummy variables
 
gen reduced_class_dim = "micro" if class_dim <= 2

replace reduced_class_dim = "piccole" if class_dim>2 & class_dim<=8

replace reduced_class_dim = "medie" if class_dim>8 & class_dim<=11

replace reduced_class_dim = "grandi" if class_dim>11 & class_dim<=14

replace reduced_class_dim = "sconosciuta" if class_dim == .

drop class_dim

// 5) INCOME_NUOVO_LAVORO

// First salary of a new job

gen income_nuovo_lavoro = income if cambio_lavoro == 1 & time != 1

// 6) ULTIMO_STIPENDIO

// This variable indicates the salary of a person the month before getting a new job

gen ultimo_stipendio = income[_n - 1] if time != 1 & cambio_lavoro == 1

// we also want to add the salary that an unemployed person got in his last job

gen ultimo_stipendio_poi_disoccupati = ultimo_stipendio if emp == 0 & emp[_n - 1] == 1

replace ultimo_stipendio_poi_disoccupati = ultimo_stipendio_poi_disoccupati[_n - 1] if ultimo_stipendio_poi_disoccupati == . & emp == 0 & emp[_n - 1] == 0

replace ultimo_stipendio = ultimo_stipendio_poi_disoccupati[_n - 1] if ultimo_stipendio == . & income_nuovo_lavoro != .

drop ultimo_stipendio_poi_disoccupati

// 7) RGDP_ADJ_INCOME_NUOVO_LAVORO

// We then adjust by Real GDP the three variables INCOME_NUOVO_LAVORO, ULTIMO_STIPENDIO, INCOME

merge m:1 anno_m using inflation&gdp_values

drop _merge HCPI Nominal_GDP

sort id_lavoratore anno_m

scalar media_REAL_GDP = 1707809

gen RGDP_adj_income_nuovo_lavoro = (income_nuovo_lavoro / Real_GDP) * media_REAL_GDP

// 8) RGDP_ADJ_ULTIMO_STIPENDIO

gen RGDP_adj_ultimo_stipendio = (ultimo_stipendio / Real_GDP) * media_REAL_GDP

// 9) RGDP_adj_income

gen RGDP_adj_income = (income / Real_GDP) * media_REAL_GDP

scalar drop media_REAL_GDP




////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////

// DESCRIPTIVE STATISTICS variable INCOME

preserve

collapse income, by(id_lavoratore)

twoway hist income if income <= 60000, bin(500) percent xlabel(0 10000 20000 30000 40000 50000 60000, valuelabel)

graph export hist_income.pdf, replace

sum income, d

// Median: €14166.95  Mean: €16804.3

restore



////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////

// DESCRIPTIVE STATISTICS QUALITATIVE variables already in the dataset

// Qualitative variables already in the dataset used in the final regressions listed in alphabetical order: age_group, macro_region, qualifica, sesso, tipo_contratto 

tab age_group

tab macro_region 

tab qualifica

// Mainly operaio (56.3%) and impiegato (35.6%). 4% apprendista.

tab qualifica if age <= 30

// Operaio still the main occupation (53.0%). Apprendista grows to 19.18% while impiegato drops to 27.53%.

tab qualifica if age > 30

// Not particularly different than with under 30. Obviously apprendista drops (to 2.1%)

// Why did we divide between younger and older than 30? Because if a person is younger than 24 its first income in the sample is much more likely to be its first income in its life compared to an older person. This is important because we will study primo_stipendio (first income appearing in the sample). Therefore now observe the observations at time == 1 (remeber, first and last time period in the sample the indivdual is always employed). What "qualifica" was such supposedly first job? 

tab qualifica if time == 1 & age <= 30

// The great majority of the are operaio (52.6%), impiegato (27.06%) ad apprendista (19.92)
 
// Does the situation change for older age_groups (and so less likely first-jobs)?

tab qualifica if time == 1 & age > 30

// Yes! The amount of apprendista drastically declines to a 0.11 percent. We can see how the great majority of workers remains operaio, with a 61.4%. We have to remember that those people are much less likely to be working their first actual job than the ones younger than 30.

tab sesso

// 61.2% male. 38.8% female

tab tipo_contratto 

// 85.8% open contracts. 14.2% temporary.



////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////

// DESCRIPTIVE STATISTICS added variables

// 1) N_LAVORI and TIMEMAX

sum n_lavori, d

// The number of jobs that an individual had throughout the dataset is pretty low. More than the 40% of the observations shows people that never change job in the dataset. Does this happen because they change few jobs or because we have many people remaining for a small interval in the dataset?

// Let's see the distribution of the time that people spend in the dataset and how much it affects the low median value of n_lavori

sum timemax if time == 1, d

hist timemax if time == 1, percent

graph export hist_timemax.pdf, replace

// we see that around 30% of people is present during the whole 12 years. However, a little ore than 10% remains only for less than a year and around 25% for less than 3 years.

sum n_lavori if timemax >= 12, d

// The situation slightly changes if we exclude those who work less than a year

sum n_lavori if timemax >= 36, d

// It changes further excluding who works less that 3 years, and the average increases.

sum n_lavori if timemax >= 144, d

// The situation changes counterintuitively if we only consider only who is present for the whole duration of the dataset. Indeed the average diminishes (from 3.94 to 3.31) therefore, who is present for the whole dataset changes on average less jobs that those who are present for only three years. We think that this might be due to different types of workers.

tab n_lavori qualifica if timemax >= 144

display 18314/24985

tab n_lavori qualifica

display 52734/96538

// We can notice how the ratio impiegati/operai in the subset of who works for 144 months (0.733) is much higher than the one of the entire dataset (0.546). If an employee changes job less frequently than a worker would do, this could explain the counterintuitive result that we obtrained before

sum n_lavori if qualifica == "operaio"

sum n_lavori if qualifica == "impiegato"

// As we supposed, the average of n_lavori of an employee (2.73) is drastically lower than the one of a worker (3.59). 

// People who change job are in total:

count if n_lavori >= 2 & n_lavori != .

// in percentage:

count if time == 1 

display 97981 / 165837

count if n_lavori >= 2 & n_lavori != . & timemax == 144

count if time == 1 & timemax == 144

display 30259 / 47186

// 59% of all individuals observed changes job at least once. The number increases if we only consider people who remain for the whole 12 years in the dataset.

// 2) PRIMO_STIPENDIO

sum primo_stipendio, d

// We can see how the average is extremely low. Moreover some values do not make much sense for example: 5 percent earns less than 300 euros ANNUALLY, 10 percent less than 800. How is this possible? Are they all "apprendista"? Let's check

sum primo_stipendio if qualifica != "apprendista", d

// The situation remains more or less similar. It is certainly not because everyone is an apprendista

// We checked the data manually and we found out that many data make zero sense. Probably some of them were monthly wages indicated wrongly as yearly. Other were values too low to even be monthly and other too low to be yearly and too high to be monthly for a certian occupation. As a result, we decided to avoid all incomes below 10000 euros in our regressions.

// 3) REDUCED_CLASS_DIM 

tab reduced_class_dim

// 4) PRIMO_STIPENDIO, INCOME_NUOVO_LAVORO, ULTIMO_STIPENDIO, RGDP_ADJ_INCOME_NUOVO_LAVORO, RGDP_ADJ_ULTIMO_STIPENDIO & RGDP_adj_income

preserve

collapse primo_stipendio income_nuovo_lavoro ultimo_stipendio RGDP_adj_income_nuovo_lavoro RGDP_adj_ultimo_stipendio RGDP_adj_income, by(id_lavoratore)

sum primo_stipendio, d

twoway hist primo_stipendio if primo_stipendio <= 60000, bin(500) percent xlabel(0 10000 20000 30000 40000 50000 60000, valuelabel)

graph export hist_primo_stipendio.pdf, replace

sum income_nuovo_lavoro, d

sum ultimo_stipendio, d

sum RGDP_adj_income_nuovo_lavoro, d

twoway hist RGDP_adj_income_nuovo_lavoro if income_nuovo_lavoro <= 60000, bin(500) percent

graph export hist_RGDP_adj_income_nuovo_lavoro.pdf, replace

sum RGDP_adj_ultimo_stipendio, d

sum RGDP_adj_income, d

twoway hist RGDP_adj_income if RGDP_adj_income <= 60000, bin(500) percent

graph export hist_RGDP_adj_income.pdf, replace

restore



////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////

// In the following part of the code we will analyze the final regressions we use. To make the code not too long and difficult to read, we will omit all regressions we did not chose but that were fundamental to reach the ones we end up using. For this reason, we decided to include the section "Setting up the models" in the research paper in order to explain why we decided to use certain variables and to pose certain constraints in the final regressions, to better expain certain choices we made

// ANALYSIS OF FIRST WAGE

 // It is important for our study to understand the components of the first wage. There is an issue: the first wage we have in this sample is not certain to be the first wage in a person's life. To account for this issue, we selected the people that are more likely to be first workers, which looking at other researches we identified as people under 30 years old. 

// We encode our qualitiative string variables in order to generate dummy variables in our OLS regression.

encode sesso, generate(Sesso) 

encode age_group, generate(Age_group) 

encode regione, generate(Regione)

encode tipo_contratto, generate(Tipo_contratto)

encode qualifica, generate(Qualifica)

encode macro_region, gen(Macro_region)

encode reduced_class_dim, gen(Reduced_class_dim)

reg primo_stipendio i.Sesso i.Age_group i.Reduced_class_dim n_lavori i.Macro_region i.Tipo_contratto i.Qualifica i.anno if primo_stipendio >= 10000 & timemax >= 36 & age <= 30

eststo m1

anova primo_stipendio i.Sesso i.Age_group i.Reduced_class_dim i.Macro_region i.Tipo_contratto i.Qualifica if primo_stipendio >= 10000 & timemax >= 36 & age <= 30

eststo m2

hetreg primo_stipendio i.Sesso i.Age_group i.Reduced_class_dim n_lavori i.Macro_region i.Tipo_contratto i.Qualifica if primo_stipendio >= 10000 & timemax >= 36 & age <= 30

eststo m3

esttab using grafici_primo_stipendio.rtf, label title(REGRESSIONS OUTPUTS PRIMO STIPENDIO) mtitles(OLS ANOVA HETREG) cells(b(fmt(0)) p(fmt(2)) ci(par fmt(0))) stats(r2 df_r bic, fmt(3 0 1) label(R-sqr dfres BIC)) replace

eststo clear

quietly reg primo_stipendio i.Sesso i.Age_group i.Reduced_class_dim n_lavori i.Macro_region i.Tipo_contratto i.Qualifica if primo_stipendio >= 10000 & timemax >= 36 & age <= 30

estat vif

estat hettest


////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////

// ANALYSIS ON SALARY GROWTH (RGDP_adj_income_nuovo_lavoro)

reg RGDP_adj_income_nuovo_lavoro RGDP_adj_ultimo_stipendio i.Sesso i.Age_group i.Reduced_class_dim i.Tipo_contratto i.Qualifica if income_nuovo_lavoro >= 10000 & n_lavori >= 2 & timemax >= 36

eststo m4

anova RGDP_adj_income_nuovo_lavoro i.Sesso i.Age_group i.Reduced_class_dim i.Tipo_contratto i.Qualifica i.anno if income_nuovo_lavoro >= 10000 & n_lavori >= 2 & timemax >= 36

eststo m5

esttab using grafici_RGDP_adj_income_nuovo_lavoro.rtf, label title(REGRESSIONS RGDP INCOME NUOVO LAVORO) mtitles(OLS ANOVA) cells(b(fmt(0)) p(fmt(2)) ci(par fmt(0))) stats(r2 df_r bic, fmt(3 0 1) label(R-sqr dfres BIC)) replace

eststo clear

quietly reg RGDP_adj_income_nuovo_lavoro RGDP_adj_ultimo_stipendio i.Sesso i.Age_group i.Reduced_class_dim i.Tipo_contratto i.Qualifica if income_nuovo_lavoro >= 10000 & n_lavori >= 2 & timemax >= 36

predict res, residuals

predict fitted, xb

rvfplot, addplot(qfit res fitted)

graph export rvplot_RGDP_adj_income_nuovo_lavoro.pdf, replace

drop res fitted

estat vif

estat hettest



////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////

// ANALYSIS ON SALARY GROWTH (income as polled, fixed effect with LSDV2 and LSDV3, random effects with GLS)


xtset id_lavoratore time

// POOLED REGRESSION

xi: reg RGDP_adj_income i.Sesso i.Age_group i.Reduced_class_dim i.Tipo_contratto i.Qualifica if income >= 10000 & timemax >= 36

eststo pooled

// PANEL DATA - FIXED EFFECT with LSDV2

// LSDV2 (least Square Dummy Variables)

xi: reg RGDP_adj_income i.Sesso i.Age_group i.Reduced_class_dim i.Tipo_contratto i.Qualifica i.time if income >= 10000 & timemax >= 36

eststo LSDV2

// Pooled vs lsdv2

// you test for the significance of the restrictions that lsdv has (i.time)

// F TEST: NULL —> FIXED EFFECTS = 0 

testparm _Itime_2 _Itime_3 _Itime_4 _Itime_5 _Itime_6 _Itime_7 _Itime_8 _Itime_9 _Itime_10 _Itime_11 _Itime_12 _Itime_13 _Itime_14 _Itime_15 _Itime_16 _Itime_17 _Itime_18 _Itime_19 _Itime_20 _Itime_21 _Itime_22 _Itime_23 _Itime_24 _Itime_25 _Itime_26 _Itime_27 _Itime_28 _Itime_29 _Itime_30 _Itime_31 _Itime_32 _Itime_33 _Itime_34 _Itime_35 _Itime_36 _Itime_37 _Itime_38 _Itime_39 _Itime_40 _Itime_41 _Itime_42 _Itime_43 _Itime_44 _Itime_45 _Itime_46 _Itime_47 _Itime_48 _Itime_49 _Itime_50 _Itime_51 _Itime_52 _Itime_53 _Itime_54 _Itime_55 _Itime_56 _Itime_57 _Itime_58 _Itime_59 _Itime_60 _Itime_61 _Itime_62 _Itime_63 _Itime_64 _Itime_65 _Itime_66 _Itime_67 _Itime_68 _Itime_69 _Itime_70 _Itime_71 _Itime_72 _Itime_73 _Itime_74 _Itime_75 _Itime_76 _Itime_77 _Itime_78 _Itime_79 _Itime_80 _Itime_81 _Itime_82 _Itime_83 _Itime_84 _Itime_85 _Itime_86 _Itime_87 _Itime_88 _Itime_89 _Itime_90 _Itime_91 _Itime_92 _Itime_93 _Itime_94 _Itime_95 _Itime_96 _Itime_97 _Itime_98 _Itime_99 _Itime_100 _Itime_101 _Itime_102 _Itime_103 _Itime_104 _Itime_105 _Itime_106 _Itime_107 _Itime_108 _Itime_109 _Itime_110 _Itime_111 _Itime_112 _Itime_113 _Itime_114 _Itime_115 _Itime_116 _Itime_117 _Itime_118 _Itime_119 _Itime_120 _Itime_121 _Itime_122 _Itime_123 _Itime_124 _Itime_125 _Itime_126 _Itime_127 _Itime_128 _Itime_129 _Itime_130 _Itime_131 _Itime_132 _Itime_133 _Itime_134 _Itime_135 _Itime_136 _Itime_137 _Itime_138 _Itime_139 _Itime_140 _Itime_141 _Itime_142 _Itime_143 _Itime_144

// LSDV2 is better than pooled 

// LSDV3

xtreg RGDP_adj_income i.Sesso i.Age_group i.Reduced_class_dim i.Tipo_contratto i.Qualifica i.time if income >= 10000 & timemax >= 36, fe

// LSDV3 even better

eststo LSDV3

// PANEL DATA ANALYSIS - RANDOM EFFECT with GLS

xtreg RGDP_adj_income i.Sesso i.Age_group i.Reduced_class_dim i.Tipo_contratto i.Qualifica if income >= 10000 & timemax >= 36

eststo random

// Comparing random and pooled using breush pagan

// NULL —> VARIANCE RE = 0

xttest0

// haussman test to find the best between fixed and random. Reject the null (p value < 0.05) = fixed better. Fail to reject the null = random better

// NULL —> COEFFICIENT DIFF = 0
// ACCEPT NULL = RE because smaller STANDARD ERRORS
// REJECT NULL = FE accounts for OMITTED VAR BIAS at a HIGHER GROUP LEVEL

hausman LSDV3 random

// we reject the null. Fixed effects with LSDV3 is better.

esttab using grafici_RGDP_adj_income.rtf, label title(REGRESSIONS RGDP INCOME) mtitles(POLLED LSDV2 LSDV3 RANDOM) cells(b(fmt(0)) p(fmt(2)) ci(par fmt(0))) stats(r2 df_r bic, fmt(3 0 1) label(R-sqr dfres BIC)) replace

eststo clear

save RESEARCH_ON_INCOME, replace
log close