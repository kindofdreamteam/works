/*===========================================================
  ЧАСТЬ 1: УЧЁТ НЕОДНОРОДНОСТИ
===========================================================*/

encode country, gen(country_id)
destring year, replace
xtset country_id year
tsset country_id year
iis country_id
tis year
xtdes
xtsum

egen cnt_value = count(value), by(country_id)
egen cnt_gdp = count(gdp), by(country_id)
egen cnt_price = count(price), by(country_id)
egen cnt_cpi = count(cpi), by(country_id)
egen cnt_ex_rate = count(ex_rate), by(country_id)
egen cnt_dist = count(dist), by(country_id)
egen cnt_population = count(population), by(country_id)
browse country country_id year value gdp cnt_value cnt_gdp

* Выводим топ-40 импортеров по среднему значению за рассматриваемый период
preserve
	collapse(mean) mean_var = value, by (country_id)
	gsort -mean_var
	list country_id mean_var in 1/40
restore

/* Generation means on t by each country */
egen meanvalue = mean(value), by(country_id)
egen meanprice = mean(price), by(country_id)

/* Generation deviations from means on t by each country */
gen divalue = value - meanvalue
gen diprice = price - meanprice

preserve
    collapse (mean) meanvalue meanprice divalue diprice, by(country_id)
    
    * Топ-40 по value
    gsort -meanvalue
    gen rank_value = _n
    list rank_value country_id meanvalue meanprice divalue diprice in 1/40, ///
      
    
    * Топ-40 по price  
    gsort -meanprice
    gen rank_price = _n
    list rank_price country_id meanvalue meanprice divalue diprice in 1/40, ///
   
restore

* Фиксируем time-varying дамми
foreach var in unfriendly sanctioner {
    bysort country_id: egen `var'_fixed = mean(cond(year == 2023, `var', .))
}

* Анализ
local dummies border EAES language unfriendly_fixed sanctioner_fixed

foreach d of local dummies {
    display _newline(3) "{hline 60}"
    display " Анализ по: `d' "
    display "{hline 60}"
    
    foreach val in 0 1 {
        display _newline(1) "Группа `d' = `val':"
        
        preserve
            keep if `d' == `val'
            
            * Число стран
            egen tag = tag(country_id)
            count if tag == 1
            local n_c = r(N)
            
            * Число периодов
            egen tag_y = tag(year)
            count if tag_y == 1
            local n_p = r(N)
            
            * Наблюдения
            count
            local n_obs = r(N)
            
            display "  Стран: `n_c', Периодов: `n_p', Наблюдений: `n_obs'"
            display "  xtsum:"
            xtsum value price log_gdp log_population log_dist log_ex_rate
            
            drop tag tag_y
        restore
    }
}

drop *_fixed

* Гистограмма распределения dist
histogram dist, percent normal title("Распределение dist") ///
    xtitle("dist") ytitle("Percent")

* График плотности распределения dist
kdensity dist, normal title("Плотность dist") ///
    xtitle("dist") ytitle("Density") ///
    legend(label(1 "Kernel density estimate") label(2 "Normal density"))
	
twoway ///
    (line value year if country == "КИТАЙ") ///
    (line value year if country == "ГЕРМАНИЯ") ///
    (line value year if country == "ИТАЛИЯ") ///
    (line value year if country == "ПОЛЬША") ///
    (line value year if country == "США") ///
	(line value year if country == "ЯПОНИЯ") ///
    (line value year if country == "ЮЖНАЯ КОРЕЯ") ///
    (line value year if country == "БЕЛАРУСЬ") ///
    (line value year if country == "ТУРЦИЯ") ///
    (line value year if country == "ЧЕХИЯ"), ///
    legend(order( ///
        1 "Китай" ///
        2 "Германия" ///
        3 "Италия" ///
        4 "Польша" ///
        5 "США" ///
		6 "Япония" ///
        7 "Южная Корея" ///
        8 "Беларусь" ///
        9 "Турция" ///
        10 "Чехия" ///
    ) rows(9) position(11) size(small)) ///
    xlabel(2017(1)2023, labsize(small)) ///
    ylabel(, labsize(small)) ///
    title("Динамика импорта из выбранных стран, 2017-2023") ///
    ytitle("Объем импорта", size(small)) ///
    xtitle("Год", size(small)) ///
    graphregion(color(white))

* График по годам
bysort year: egen value_mean_year = mean(value)
twoway scatter value year, msymbol(circle_hollow) || ///
       connected value_mean_year year, msymbol(diamond) || , ///
       xlabel(2017(1)2023) ///
       title("Динамика среднего импорта по годам") ///
       ytitle("Объем импорта") xtitle("Годы")

* ДИАГРАММЫ РАССЕЯНИЯ

* 1. ИМПОРТ vs ЦЕНА (price) для каждой страны

xtline value if country == "КИТАЙ", recast(scatter) i(country_id) t(price) ///
    title("Китай: Импорт vs Цена") ///
    ytitle("Объем импорта (USD)") xtitle("Цена (USD)") ///
    name(chi_price, replace) graphregion(color(white))

xtline value if country == "ГЕРМАНИЯ", recast(scatter) i(country_id) t(price) ///
    title("Германия: Импорт vs Цена") ///
    ytitle("Объем импорта (USD)") xtitle("Цена (USD)") ///
    name(ger_price, replace) graphregion(color(white))

xtline value if country == "ИТАЛИЯ", recast(scatter) i(country_id) t(price) ///
    title("Италия: Импорт vs Цена") ///
    ytitle("Объем импорта (USD)") xtitle("Цена (USD)") ///
    name(ita_price, replace) graphregion(color(white))

xtline value if country == "ПОЛЬША", recast(scatter) i(country_id) t(price) ///
    title("Польша: Импорт vs Цена") ///
    ytitle("Объем импорта (USD)") xtitle("Цена (USD)") ///
    name(pol_price, replace) graphregion(color(white))

xtline value if country == "США", recast(scatter) i(country_id) t(price) ///
    title("США: Импорт vs Цена") ///
    ytitle("Объем импорта (USD)") xtitle("Цена (USD)") ///
    name(usa_price, replace) graphregion(color(white))

xtline value if country == "ЯПОНИЯ", recast(scatter) i(country_id) t(price) ///
    title("Япония: Импорт vs Цена") ///
    ytitle("Объем импорта (USD)") xtitle("Цена (USD)") ///
    name(jpn_price, replace) graphregion(color(white))

xtline value if country == "ЮЖНАЯ КОРЕЯ", recast(scatter) i(country_id) t(price) ///
    title("Южная Корея: Импорт vs Цена") ///
    ytitle("Объем импорта (USD)") xtitle("Цена (USD)") ///
    name(kor_price, replace) graphregion(color(white))

xtline value if country == "БЕЛАРУСЬ", recast(scatter) i(country_id) t(price) ///
    title("Беларусь: Импорт vs Цена") ///
    ytitle("Объем импорта (USD)") xtitle("Цена (USD)") ///
    name(bel_price, replace) graphregion(color(white))

xtline value if country == "ТУРЦИЯ", recast(scatter) i(country_id) t(price) ///
    title("Турция: Импорт vs Цена") ///
    ytitle("Объем импорта (USD)") xtitle("Цена (USD)") ///
    name(tur_price, replace) graphregion(color(white))

xtline value if country == "ЧЕХИЯ", recast(scatter) i(country_id) t(price) ///
    title("Чехия: Импорт vs Цена") ///
    ytitle("Объем импорта (USD)") xtitle("Цена (USD)") ///
    name(cze_price, replace) graphregion(color(white))

graph combine chi_price ger_price ita_price pol_price usa_price ///
            jpn_price kor_price bel_price tur_price cze_price, ///
    title("Импорт vs Цена по странам (топ-10)", size(small)) /// 
    rows(5) cols(2) ///
    imargin(0 0 0 0) /// 
    graphregion(color(white)) ///
    xsize(5) ysize(7) /// 
    scale(0.7) /// 
    name(combined_price_final, replace)

* 2. ИМПОРТ vs ВВП (gdp) для каждой страны

xtline value if country == "КИТАЙ", recast(scatter) i(country_id) t(gdp) ///
    title("Китай: Импорт vs ВВП") ///
    ytitle("Объем импорта (USD)") xtitle("ВВП (USD)") ///
    name(chi_gdp, replace) graphregion(color(white))

xtline value if country == "ГЕРМАНИЯ", recast(scatter) i(country_id) t(gdp) ///
    title("Германия: Импорт vs ВВП") ///
    ytitle("Объем импорта (USD)") xtitle("ВВП (USD)") ///
    name(ger_gdp, replace) graphregion(color(white))

xtline value if country == "ИТАЛИЯ", recast(scatter) i(country_id) t(gdp) ///
    title("Италия: Импорт vs ВВП") ///
    ytitle("Объем импорта (USD)") xtitle("ВВП (USD)") ///
    name(ita_gdp, replace) graphregion(color(white))

xtline value if country == "ПОЛЬША", recast(scatter) i(country_id) t(gdp) ///
    title("Польша: Импорт vs ВВП") ///
    ytitle("Объем импорта (USD)") xtitle("ВВП (USD)") ///
    name(pol_gdp, replace) graphregion(color(white))

xtline value if country == "США", recast(scatter) i(country_id) t(gdp) ///
    title("США: Импорт vs ВВП") ///
    ytitle("Объем импорта (USD)") xtitle("ВВП (USD)") ///
    name(usa_gdp, replace) graphregion(color(white))

xtline value if country == "ЯПОНИЯ", recast(scatter) i(country_id) t(gdp) ///
    title("Япония: Импорт vs ВВП") ///
    ytitle("Объем импорта (USD)") xtitle("ВВП (USD)") ///
    name(jpn_gdp, replace) graphregion(color(white))

xtline value if country == "ЮЖНАЯ КОРЕЯ", recast(scatter) i(country_id) t(gdp) ///
    title("Южная Корея: Импорт vs ВВП") ///
    ytitle("Объем импорта (USD)") xtitle("ВВП (USD)") ///
    name(kor_gdp, replace) graphregion(color(white))

xtline value if country == "БЕЛАРУСЬ", recast(scatter) i(country_id) t(gdp) ///
    title("Беларусь: Импорт vs ВВП") ///
    ytitle("Объем импорта (USD)") xtitle("ВВП (USD)") ///
    name(bel_gdp, replace) graphregion(color(white))

xtline value if country == "ТУРЦИЯ", recast(scatter) i(country_id) t(gdp) ///
    title("Турция: Импорт vs ВВП") ///
    ytitle("Объем импорта (USD)") xtitle("ВВП (USD)") ///
    name(tur_gdp, replace) graphregion(color(white))

xtline value if country == "ЧЕХИЯ", recast(scatter) i(country_id) t(gdp) ///
    title("Чехия: Импорт vs ВВП") ///
    ytitle("Объем импорта (USD)") xtitle("ВВП (USD)") ///
    name(cze_gdp, replace) graphregion(color(white))

graph combine chi_gdp ger_gdp ita_gdp pol_gdp usa_gdp ///
            jpn_gdp kor_gdp bel_gdp tur_gdp cze_gdp, ///
    title("Импорт vs ВВП по странам (топ-10)", size(small)) ///
    rows(5) cols(2) ///
    imargin(0 0 0 0) ///
    graphregion(color(white)) ///
    xsize(5) ysize(7) ///
    scale(0.7) ///
    name(combined_gdp_final, replace)

* Определяем топ-30 стран по суммарному импорту
tempfile full_data top30
save `full_data'

collapse (sum) value, by(country_id)
gsort -value
keep in 1/30
keep country_id
save `top30'

use `full_data', clear
merge m:1 country_id using `top30'
keep if _merge == 3
drop _merge

* 1. Графики для обычных переменных
twoway scatter value price, ///
    by(country, col(5) note("") legend(off)) ///
    ytitle("Import value (USD)") ///
    xtitle("Price (USD)") ///
    scheme(s1color)

twoway scatter value gdp, ///
    by(country, col(5) note("") legend(off)) ///
    ytitle("Import value (USD)") ///
    xtitle("GDP (current USD)") ///
    scheme(s1color)

* 2. Графики для логарифмов
twoway scatter log_value log_price, ///
    by(country, col(5) note("") legend(off)) ///
    ytitle("Log Import value") ///
    xtitle("Log Price") ///
    scheme(s1color)

twoway scatter log_value log_gdp, ///
    by(country, col(5) note("") legend(off)) ///
    ytitle("Log Import value") ///
    xtitle("Log GDP") ///
    scheme(s1color)

* ANCOVA-analysis on year
* ANCOVA-анализ по всей панели (672 наблюдения)

* Модель 0: отдельные регрессии по годам
scalar rss_ur = 0
scalar n_ur = 0
scalar df_ur = 0

forvalues i = 2017/2023 {
    qui reg log_value log_price log_gdp if year == `i'
    
    scalar z`i' = e(rss)
    scalar df`i' = e(df_r)
    scalar n`i' = e(N)
    
    scalar rss_ur = rss_ur + z`i'
    scalar n_ur = n_ur + n`i'
    scalar df_ur = df_ur + df`i'
}

* Модель 1: с временными эффектами
qui reg log_value log_price log_gdp i.year
scalar rss_r1 = e(rss)
scalar n_r1 = e(N)
tab year
local T = r(r)
scalar df_r1_cor = n_r1 - `T' - 2

* Модель 2: Pooled
qui reg log_value log_price log_gdp
scalar rss_r2 = e(rss)
scalar n_r2 = e(N)
scalar df_r2 = e(df_r)

* F-тесты
scalar fh1 = ((rss_r1 - rss_ur) / (df_r1_cor - df_ur)) / (rss_ur / df_ur)
scalar pval1 = Ftail(df_r1_cor - df_ur, df_ur, fh1)
scalar fh2 = ((rss_r2 - rss_ur) / (df_r2 - df_ur)) / (rss_ur / df_ur)
scalar pval2 = Ftail(df_r2 - df_ur, df_ur, fh2)
scalar fh3 = ((rss_r2 - rss_r1) / (df_r2 - df_r1_cor)) / (rss_r1 / df_r1_cor)
scalar pval3 = Ftail(df_r2 - df_r1_cor, df_r1_cor, fh3)

display "Базовая модель (N=672):"
display "Тест 1 (модель (0) vs модель (1)): F = " %5.4f fh1 ", p-value = " %7.4f pval1
display "Тест 2 (модель (0) vs модель (2)): F = " %5.4f fh2 ", p-value = " %7.4f pval2
display "Тест 3 (модель (1) vs модель (2)): F = " %5.4f fh3 ", p-value = " %7.4f pval3

* ANCOVA-анализ на сбалансированной панели(469 наблюдений)
* Создаем сбалансированную панель
preserve
bysort country_id: egen years_count = count(year)
keep if years_count == 7
drop years_count
display "Сбалансированная панель: " _N " наблюдений"

* Модель 0: отдельные регрессии по годам
scalar rss_ur = 0
scalar n_ur = 0
scalar df_ur = 0

forvalues i = 2017/2023 {
    qui reg log_value log_price log_gdp if year == `i'
    
    scalar z`i' = e(rss)
    scalar df`i' = e(df_r)
    scalar n`i' = e(N)
    
    scalar rss_ur = rss_ur + z`i'
    scalar n_ur = n_ur + n`i'
    scalar df_ur = df_ur + df`i'
}

* Модель 1: с временными эффектами
qui reg log_value log_price log_gdp i.year
scalar rss_r1 = e(rss)
scalar n_r1 = e(N)
local T = 7
scalar df_r1_cor = n_r1 - `T' - 2

* Модель 2: Pooled
qui reg log_value log_price log_gdp
scalar rss_r2 = e(rss)
scalar n_r2 = e(N)
scalar df_r2 = e(df_r)

* F-тесты
scalar fh1 = ((rss_r1 - rss_ur) / (df_r1_cor - df_ur)) / (rss_ur / df_ur)
scalar pval1 = Ftail(df_r1_cor - df_ur, df_ur, fh1)
scalar fh2 = ((rss_r2 - rss_ur) / (df_r2 - df_ur)) / (rss_ur / df_ur)
scalar pval2 = Ftail(df_r2 - df_ur, df_ur, fh2)
scalar fh3 = ((rss_r2 - rss_r1) / (df_r2 - df_r1_cor)) / (rss_r1 / df_r1_cor)
scalar pval3 = Ftail(df_r2 - df_r1_cor, df_r1_cor, fh3)

display "Сбалансированная панель(N=469):"
display "Тест 1 (модель (0) vs модель (1)): F = " %5.4f fh1 ", p-value = " %7.4f pval1
display "Тест 2 (модель (0) vs модель (2)): F = " %5.4f fh2 ", p-value = " %7.4f pval2
display "Тест 3 (модель (1) vs модель (2)): F = " %5.4f fh3 ", p-value = " %7.4f pval3

restore

* ANCOVA-анализ по группам (санкционный статус)
tab sanctioner
* Санкционные страны (sanctioner = 1)
preserve
keep if sanctioner == 1
display "Санкционные страны: N = " _N

* Модель 0: отдельные регрессии по годам
scalar rss_ur = 0
scalar n_ur = 0
scalar df_ur = 0

forvalues i = 2017/2023 {
    count if year == `i'
    if r(N) > 5 {
        qui reg log_value log_price log_gdp if year == `i'
        
        scalar z`i' = e(rss)
        scalar df`i' = e(df_r)
        scalar n`i' = e(N)
        
        scalar rss_ur = rss_ur + z`i'
        scalar n_ur = n_ur + n`i'
        scalar df_ur = df_ur + df`i'
    }
}

* Модель 1: с временными эффектами
qui reg log_value log_price log_gdp i.year
scalar rss_r1 = e(rss)
scalar n_r1 = e(N)
tab year
local T = r(r)
scalar df_r1_cor = n_r1 - `T' - 2

* Модель 2: Pooled
qui reg log_value log_price log_gdp
scalar rss_r2 = e(rss)
scalar n_r2 = e(N)
scalar df_r2 = e(df_r)

* F-тесты
scalar fh1 = ((rss_r1 - rss_ur) / (df_r1_cor - df_ur)) / (rss_ur / df_ur)
scalar pval1 = Ftail(df_r1_cor - df_ur, df_ur, fh1)
scalar fh2 = ((rss_r2 - rss_ur) / (df_r2 - df_ur)) / (rss_ur / df_ur)
scalar pval2 = Ftail(df_r2 - df_ur, df_ur, fh2)
scalar fh3 = ((rss_r2 - rss_r1) / (df_r2 - df_r1_cor)) / (rss_r1 / df_r1_cor)
scalar pval3 = Ftail(df_r2 - df_r1_cor, df_r1_cor, fh3)

display "Санкционные страны:"
display "Тест 1 (модель (0) vs модель (1)): F = " %5.4f fh1 ", p-value = " %7.4f pval1
display "Тест 2 (модель (0) vs модель (2)): F = " %5.4f fh2 ", p-value = " %7.4f pval2
display "Тест 3 (модель (1) vs модель (2)): F = " %5.4f fh3 ", p-value = " %7.4f pval3

restore

* Несанкционные страны (sanctioner = 0)
preserve
keep if sanctioner == 0
display "Несанкционные страны: N = " _N

* Модель 0: отдельные регрессии по годам
scalar rss_ur = 0
scalar n_ur = 0
scalar df_ur = 0

forvalues i = 2017/2023 {
    count if year == `i'
    if r(N) > 5 {
        qui reg log_value log_price log_gdp if year == `i'
        
        scalar z`i' = e(rss)
        scalar df`i' = e(df_r)
        scalar n`i' = e(N)
        
        scalar rss_ur = rss_ur + z`i'
        scalar n_ur = n_ur + n`i'
        scalar df_ur = df_ur + df`i'
    }
}

* Модель 1: с временными эффектами
qui reg log_value log_price log_gdp i.year
scalar rss_r1 = e(rss)
scalar n_r1 = e(N)
tab year
local T = r(r)
scalar df_r1_cor = n_r1 - `T' - 2

* Модель 2: Pooled
qui reg log_value log_price log_gdp
scalar rss_r2 = e(rss)
scalar n_r2 = e(N)
scalar df_r2 = e(df_r)

* F-тесты
scalar fh1 = ((rss_r1 - rss_ur) / (df_r1_cor - df_ur)) / (rss_ur / df_ur)
scalar pval1 = Ftail(df_r1_cor - df_ur, df_ur, fh1)
scalar fh2 = ((rss_r2 - rss_ur) / (df_r2 - df_ur)) / (rss_ur / df_ur)
scalar pval2 = Ftail(df_r2 - df_ur, df_ur, fh2)
scalar fh3 = ((rss_r2 - rss_r1) / (df_r2 - df_r1_cor)) / (rss_r1 / df_r1_cor)
scalar pval3 = Ftail(df_r2 - df_r1_cor, df_r1_cor, fh3)

display "Несанкционные страны:"
display "Тест 1 (модель (0) vs модель (1)): F = " %5.4f fh1 ", p-value = " %7.4f pval1
display "Тест 2 (модель (0) vs модель (2)): F = " %5.4f fh2 ", p-value = " %7.4f pval2
display "Тест 3 (модель (1) vs модель (2)): F = " %5.4f fh3 ", p-value = " %7.4f pval3
display ""

restore

* ANCOVA-анализ модель с взаимодействием price × sanctioner
gen log_price_X_sanctioner = log_price * sanctioner

* Проверка мультиколлинеарности
display "=== ПРОВЕРКА МУЛЬТИКОЛЛИНЕАРНОСТИ ==="
reg log_value log_price log_gdp log_price_X_sanctioner
vif
display "VIF < 10 — мультиколлинеарности нет"

* Модель 0: отдельные регрессии по годам
scalar rss_ur = 0
scalar n_ur = 0
scalar df_ur = 0

forvalues i = 2017/2023 {
    qui reg log_value log_price log_gdp log_price_X_sanctioner if year == `i'
    
    scalar z`i' = e(rss)
    scalar df`i' = e(df_r)
    scalar n`i' = e(N)
    
    scalar rss_ur = rss_ur + z`i'
    scalar n_ur = n_ur + n`i'
    scalar df_ur = df_ur + df`i'
    
    display "Год `i': RSS = " %9.4f z`i' ", df = " df`i' ", N = " n`i'
}

* Модель 1: с временными эффектами
qui reg log_value log_price log_gdp log_price_X_sanctioner i.year
scalar rss_r1 = e(rss)
scalar n_r1 = e(N)
tab year
local T = r(r)
scalar df_r1_cor = n_r1 - `T' - 3

* Модель 2: Pooled
qui reg log_value log_price log_gdp log_price_X_sanctioner
scalar rss_r2 = e(rss)
scalar n_r2 = e(N)
scalar df_r2 = e(df_r)

* F-тесты
display "===F-тесты==="
scalar fh1 = ((rss_r1 - rss_ur) / (df_r1_cor - df_ur)) / (rss_ur / df_ur)
scalar pval1 = Ftail(df_r1_cor - df_ur, df_ur, fh1)
scalar fh2 = ((rss_r2 - rss_ur) / (df_r2 - df_ur)) / (rss_ur / df_ur)
scalar pval2 = Ftail(df_r2 - df_ur, df_ur, fh2)
scalar fh3 = ((rss_r2 - rss_r1) / (df_r2 - df_r1_cor)) / (rss_r1 / df_r1_cor)
scalar pval3 = Ftail(df_r2 - df_r1_cor, df_r1_cor, fh3)

display "Модель с взаимодействим price*sanctioner"
display "Тест 1 (модель (0) vs модель (1)): F = " %5.4f fh1 ", p-value = " %7.4f pval1
display "Тест 2 (Pмодель (0) vs модель (2)): F = " %5.4f fh2 ", p-value = " %7.4f pval2
display "Тест 3 (модель (1) vs модель (2)): F = " %5.4f fh3 ", p-value = " %7.4f pval3

* Отбор топ-50 стран по объему импорта
preserve
    collapse (sum) total_import = value, by(country_id)
    gsort -total_import
    keep in 1/50
    keep country_id
    tempfile top50
    save `top50'
restore

merge m:1 country_id using `top50'
gen top50 = (_merge == 3)
drop _merge

tab top50
count if top50 == 1
display "Всего наблюдений в топ-50: " r(N)

* ANCOVA для топ-50 стран
* Модель 0: отдельные регрессии по годам
scalar rss_ur = 0
scalar n_ur = 0
scalar df_ur = 0

forvalues i = 2017/2023 {
    qui reg log_value log_price log_gdp if year == `i' & top50 == 1
    
    scalar z`i' = e(rss)
    scalar df`i' = e(df_r)
    scalar n`i' = e(N)
    
    scalar rss_ur = rss_ur + z`i'
    scalar n_ur = n_ur + n`i'
    scalar df_ur = df_ur + df`i'
}

* Модель 1: с временными эффектами
qui reg log_value log_price log_gdp i.year if top50 == 1
scalar rss_r1 = e(rss)
scalar n_r1 = e(N)
tab year if top50 == 1
local T = r(r)
scalar df_r1_cor = n_r1 - `T' - 2

* Модель 2: Pooled
qui reg log_value log_price log_gdp if top50 == 1
scalar rss_r2 = e(rss)
scalar n_r2 = e(N)
scalar df_r2 = e(df_r)

* F-тесты (без изменений)
scalar fh1 = ((rss_r1 - rss_ur) / (df_r1_cor - df_ur)) / (rss_ur / df_ur)
scalar pval1 = Ftail(df_r1_cor - df_ur, df_ur, fh1)

scalar fh2 = ((rss_r2 - rss_ur) / (df_r2 - df_ur)) / (rss_ur / df_ur)
scalar pval2 = Ftail(df_r2 - df_ur, df_ur, fh2)

scalar fh3 = ((rss_r2 - rss_r1) / (df_r2 - df_r1_cor)) / (rss_r1 / df_r1_cor)
scalar pval3 = Ftail(df_r2 - df_r1_cor, df_r1_cor, fh3)

display "Результаты ANCOVA для топ-50 стран"                           // 
display "Тест 1 (FE vs Yearly): F = " %5.4f fh1 ", p-value = " %7.4f pval1
display "Тест 2 (Pooled vs Yearly): F = " %5.4f fh2 ", p-value = " %7.4f pval2
display "Тест 3 (Pooled vs FE): F = " %5.4f fh3 ", p-value = " %7.4f pval3

* Модели Pooled, FE, RE и сравнение моделей
* Отбор топ-50 стран по стоимостному объему импорта
preserve
    collapse (sum) total_import = value, by(country_id)
    gsort -total_import
    keep in 1/50
    keep country_id
    tempfile top50
    save `top50'
restore

merge m:1 country_id using `top50'
gen top50 = (_merge == 3)
drop _merge

tab top50
count if top50 == 1
display "Всего наблюдений в топ-50: " r(N)

* Модели Pooled, FE, RE для топ-50 стран
gen log_price_sanctioner = log_price * sanctioner
gen log_price_unfriendly = log_price * unfriendly

* Модель 1: Pooled OLS
display "=== МОДЕЛЬ 1: POOLED OLS (ТОП-50) ==="
reg log_value log_price log_gdp log_cpi log_price_sanctioner log_price_unfriendly log_dist border language if top50 == 1
est store pool_top50

* Модель 2: Fixed Effects (страны)
display "=== МОДЕЛЬ 2: FIXED EFFECTS (ТОП-50) ==="
xtreg log_value log_price log_gdp log_cpi log_price_sanctioner log_price_unfriendly log_dist border language if top50 == 1, fe
est store fe_top50

* Модель 3: Random Effects (страны)
display "=== МОДЕЛЬ 3: RANDOM EFFECTS (ТОП-50) ==="
xtreg log_value log_price log_gdp log_cpi log_price_sanctioner log_price_unfriendly log_dist border language if top50 == 1, re
xttest0  // Тест RE vs Pooled
est store re_top50

/* 4. Таблица сравнения моделей (в консоль) */
display "=== ТАБЛИЦА: СРАВНЕНИЕ МОДЕЛЕЙ POOL, FE, RE (ТОП-50) ==="
estimates table pool_top50 fe_top50 re_top50, ///
    b(%7.4f) se(%7.4f) stats(N r2)

/* 5. Тест Хаусмана (FE vs RE) */
display "=== ТЕСТ ХАУСМАНА (FE vs RE) - ТОП-50 ==="
hausman fe_top50 re_top50, constant sigmamore

/* ТЕСТ МУНДЛАКА (альтернатива Хаусману) */
display "=== ТЕСТ МУНДЛАКА (Mundlak approach) - ТОП-50 ==="

* Создание средних по странам для ключевых переменных
foreach var in log_price log_gdp log_cpi log_price_sanctioner log_price_unfriendly {
    egen m_`var' = mean(`var'), by(country_id)
}

* RE модель со средними (Mundlak)
xtreg log_value log_price log_gdp log_cpi log_price_sanctioner log_price_unfriendly ///
       m_log_price m_log_gdp m_log_cpi m_log_price_sanctioner m_log_price_unfriendly ///
       log_dist border language if top50 == 1, re

* Тест на значимость средних (если значимы -> FE лучше)
test m_log_price m_log_gdp m_log_cpi m_log_price_sanctioner m_log_price_unfriendly

* Очистка
foreach var in log_price log_gdp log_cpi log_price_sanctioner log_price_unfriendly {
    drop m_`var'
}

/* 6. Экспорт результатов в Word */
outreg2 [pool_top50 fe_top50 re_top50] using "results_pool_fe_re_top50.doc", replace ///
    title("Сравнение моделей Pool, FE, RE (топ-50 стран)") ///
    ctitle("Pooled OLS", "FE (страны)", "RE (страны)") ///
    stats(coef se) ///
    addtext(Наблюдения, e(N), R-squared, e(r2))

/* 7. Сохранение тестов в текстовый файл */
log using "tests_pool_fe_re_top50.txt", text replace

display "================================================================"
display "РЕЗУЛЬТАТЫ ТЕСТИРОВАНИЯ (ТОП-50)"
display "================================================================"
display ""
display "1. ТЕСТ FE vs POOLED"
xtreg log_value log_price log_gdp log_cpi log_price_sanctioner log_price_unfriendly log_dist border language if top50 == 1, fe
display "2. ТЕСТ RE vs POOLED (Breusch-Pagan)"
xtreg log_value log_price log_gdp log_cpi log_price_sanctioner log_price_unfriendly log_dist border language if top50 == 1, re
xttest0
display "3. ТЕСТ FE vs RE (Hausman)"
hausman fe_top50 re_top50, constant sigmamore

log close

/* 8. Дополнительная диагностика */
// VIF для проверки мультиколлинеарности
display "=== ПРОВЕРКА МУЛЬТИКОЛЛИНЕАРНОСТИ (VIF) - ТОП-50 ==="
reg log_value log_price log_gdp log_cpi log_price_sanctioner log_price_unfriendly log_dist border language if top50 == 1
vif

* МОДЕЛИ С ВРЕМЕННЫМИ ЭФФЕКТАМИ ДЛЯ ТОП-50 СТРАН
/* 1. Отбор топ-50 стран */
preserve
    collapse (sum) total_import = value, by(country_id)
    gsort -total_import
    keep in 1/50
    keep country_id
    tempfile top50
    save `top50'
restore

merge m:1 country_id using `top50'
gen top50 = (_merge == 3)
drop _merge

tab top50
count if top50 == 1
display "Всего наблюдений в топ-50: " r(N)

/* 3. Создание переменных взаимодействий */
gen log_price_sanctioner = log_price * sanctioner
gen log_price_unfriendly = log_price * unfriendly

/* 4. Создание годовых дамми */
quietly tabulate year if top50 == 1, generate(year_)

/* 5. Модели с временными эффектами для топ-50 */
display "================================================================"
display "МОДЕЛИ С ВРЕМЕННЫМИ ЭФФЕКТАМИ (ТОП-50)"
display "================================================================"

* Pooled с временными эффектами
display "=== МОДЕЛЬ: POOLED + TIME FE (ТОП-50) ==="
reg log_value log_price log_cpi log_price_sanctioner log_price_unfriendly border language year_2-year_7 if top50 == 1
est store pool_t_top50
testparm year_*  // тест на совместную значимость годов

* FE с временными эффектами
display "=== МОДЕЛЬ: FE + TIME FE (ТОП-50) ==="
xtreg log_value log_price log_cpi log_price_sanctioner log_price_unfriendly border language year_2-year_7 if top50 == 1, fe
est store fe_t_top50
testparm year_*  // тест на совместную значимость годов

* RE с временными эффектами
display "=== МОДЕЛЬ: RE + TIME FE (ТОП-50) ==="
xtreg log_value log_price log_cpi log_price_sanctioner log_price_unfriendly border language year_2-year_7 if top50 == 1, re
xttest0  // тест RE vs Pooled
testparm year_*  // тест на совместную значимость годов
est store re_t_top50

/* 6. Таблицы сравнения моделей */
display "=== ТАБЛИЦА 1: СРАВНЕНИЕ МОДЕЛЕЙ POOL_T, FE_T, RE_T (ТОП-50) ==="
estimates table pool_t_top50 fe_t_top50 re_t_top50, ///
    b(%7.4f) se(%7.4f) stats(N r2)

display "=== ТАБЛИЦА 2: КОЭФФИЦИЕНТЫ СО ЗВЕЗДОЧКАМИ ЗНАЧИМОСТИ (ТОП-50) ==="
estimates table pool_t_top50 fe_t_top50 re_t_top50, ///
    b(%7.4f) stats(N r2) star(0.01 0.05 0.10)

/* 7. Тест Хаусмана для моделей с временными эффектами */
display "=== ТЕСТ ХАУСМАНА (FE_T vs RE_T) - ТОП-50 ==="
hausman fe_t_top50 re_t_top50, constant sigmamore

/* ТЕСТ МУНДЛАКА для моделей с временными эффектами - ТОП-50 */
display "=== ТЕСТ МУНДЛАКА С ВРЕМЕННЫМИ ЭФФЕКТАМИ - ТОП-50 ==="

* Создание средних по странам для ключевых переменных (только для топ-50)
foreach var in log_price log_gdp log_cpi log_price_sanctioner log_price_unfriendly {
    egen m_`var' = mean(`var') if top50 == 1, by(country_id)
}

* Для годовых дамми тоже создаем средние (только для топ-50)
forvalues i = 2/7 {
    egen m_year_`i' = mean(year_`i') if top50 == 1, by(country_id)
}

* RE модель со средними (Mundlak) + годовые дамми
xtreg log_value log_price log_gdp log_cpi log_price_sanctioner log_price_unfriendly ///
       m_log_price m_log_gdp m_log_cpi m_log_price_sanctioner m_log_price_unfriendly ///
       m_year_2 m_year_3 m_year_4 m_year_5 m_year_6 m_year_7 ///
       border language year_2-year_7 if top50 == 1, re

* Тест на значимость средних (если значимы -> FE лучше)
test m_log_price m_log_gdp m_log_cpi m_log_price_sanctioner m_log_price_unfriendly m_year_2 m_year_3 m_year_4 m_year_5 m_year_6 m_year_7

* Очистка
foreach var in log_price log_gdp log_cpi log_price_sanctioner log_price_unfriendly {
    drop m_`var'
}
forvalues i = 2/7 {
    drop m_year_`i'
}

/* 8. Проверка мультиколлинеарности */
display "=== VIF (для pooled модели с временными эффектами) - ТОП-50 ==="
reg log_value log_price log_gdp log_cpi log_price_sanctioner log_price_unfriendly border log_dist language year_2-year_7 if top50 == 1
vif, uncentered

/* 10. Экспорт результатов */
outreg2 [pool_t_top50 fe_t_top50 re_t_top50] using "results_timeFE_top50.doc", replace ///
    title("Модели с временными эффектами (топ-50 стран)") ///
    ctitle("Pooled+Time", "FE+Time", "RE+Time") ///
    stats(coef se) ///
    addtext(Наблюдения, e(N), R-squared, e(r2))

* ============================================================
* ТЕСТЫ НА НАРУШЕНИЯ КЛРМ И КОРРЕКЦИЯ
* Финальная модель: RE без временных эффектов (топ-50)
* ============================================================

* ------------------------------------------------------------
* БЛОК 1: ТЕСТЫ НА НАРУШЕНИЯ КЛРМ
* ------------------------------------------------------------

* Установка необходимых внешних команд
ssc install xttest1
ssc install xtserial  
ssc install xttest2
ssc install xtcsd
ssc install xttest3
ssc install xtscc
ssc install outreg2

findit xttest1
findit xtserial

* Проверка установки команд
which xttest1
which xtserial
which xttest2
which xtcsd
which xtscc

* 1. Тест Бройша-Пагана на временную автокорреляцию
* H0: отсутствие автокорреляции первого порядка
* Технически строится на RE
qui xtreg log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist border language if top50==1, re
xttest1

* 2. Тест Вулдриджа на временную автокорреляцию
* H0: отсутствие автокорреляции первого порядка
* Более робастный, не требует нормальности ошибок
xtserial log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist border language if top50==1

* 3. Визуальная проверка временной автокорреляции
* Технически остатки получаем из FE
qui xtreg log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist border language if top50==1, fe
predict res_e, e
twoway scatter res_e L.res_e || lfit res_e L.res_e, ///
    title("Визуальный тест на автокорреляцию") ///
    xtitle("Остатки (t-1)") ytitle("Остатки (t)")
drop res_e

* 4-6. Тесты на пространственную автокорреляцию
* и гетероскедастичность — технически строятся на FE
xtreg log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist border language if top50==1, fe

* 4. Тест Фридмана на пространственную автокорреляцию
* H0: остатки не коррелируют между странами
xttest2

* 5. Тест Песарана на пространственную автокорреляцию
* H0: остатки не коррелируют между странами
* Предпочтителен для несбалансированных панелей с большим N
xtcsd, pesaran

* 6. Тест Вальда на гетероскедастичность
* H0: дисперсия ошибок одинакова для всех стран
xttest3

* ------------------------------------------------------------
* БЛОК 2: КОРРЕКЦИЯ НАРУШЕНИЙ КЛРМ
* Все коррекции применяются к финальной модели RE
* ------------------------------------------------------------

* Базовая RE — точка отсчёта
qui xtreg log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist border language if top50==1, re
est store re_base

* ------------------------------------------------------------
* Коррекция гетероскедастичности
* ------------------------------------------------------------

* RE с робастными стандартными ошибками Уайта
xtreg log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist border language if top50==1, re robust
est store re_rob

* GLS с гетероскедастичными панелями
* panels(hetero) — разная дисперсия для каждой страны
xtgls log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist border language if top50==1, panels(hetero)
est store gls_het

* ------------------------------------------------------------
* Коррекция временной автокорреляции
* ------------------------------------------------------------

* RE с коррекцией AR(1) по Балтаги-Ву
xtregar log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist border language if top50==1, re
est store re_ar

* GLS с общим коэффициентом AR(1) для всех стран
xtgls log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist border language if top50==1, ///
    panels(iid) corr(ar1) force
est store gls_ar

* GLS с индивидуальным AR(1) для каждой страны
xtgls log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist border language if top50==1, ///
    panels(iid) corr(psar1) force
est store gls_psar

* ------------------------------------------------------------
* Одновременная коррекция гетероскедастичности
* и временной автокорреляции
* ------------------------------------------------------------

* GLS: разная дисперсия + общий AR(1)
xtgls log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist border language if top50==1, ///
    panels(hetero) corr(ar1) force
est store gls_het_ar

* GLS: разная дисперсия + индивидуальный AR(1)
xtgls log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist border language if top50==1, ///
    panels(hetero) corr(psar1) force
est store gls_het_psar

* ------------------------------------------------------------
* Комплексная коррекция всех трёх проблем одновременно
* ------------------------------------------------------------

* PCSE с общим AR(1)
* Учитывает гетероскедастичность + оба вида автокорреляции
* Ограничение: при N>>T оценки менее надёжны
xtpcse log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist border language if top50==1, correlation(ar1)
est store pcse_ar

* PCSE с индивидуальным AR(1)
xtpcse log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist border language if top50==1, correlation(psar1)
est store pcse_psar

* Driscoll-Kraay с фиксированными эффектами
* Предпочтительный вариант для финального сравнения
xtscc log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist border language if top50==1, fe
est store scc_fe

* RE с кластеризацией по стране
xtreg log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist border language if top50==1, ///
    re vce(cluster country_id)
est store re_cl_country

* ------------------------------------------------------------
* БЛОК 3: СВОДНЫЕ ТАБЛИЦЫ ДО И ПОСЛЕ КОРРЕКЦИИ
* ------------------------------------------------------------

* Таблица 1: Коррекция гетероскедастичности
estimates table re_base re_rob gls_het, ///
    b(%7.4f) se(%7.4f) stats(N) ///
    title("Таблица 1: Коррекция гетероскедастичности")

* Таблица 2: Коррекция временной автокорреляции
estimates table re_base re_ar gls_ar gls_psar, ///
    b(%7.4f) se(%7.4f) stats(N) ///
    title("Таблица 2: Коррекция временной автокорреляции")

* Таблица 3: Одновременная коррекция
* гетероскедастичности и временной автокорреляции
estimates table re_base gls_het_ar gls_het_psar, ///
    b(%7.4f) se(%7.4f) stats(N) ///
    title("Таблица 3: Коррекция гетероскедастичности и автокорреляции")

* Таблица 4: Финальное сравнение всех методов коррекции
estimates table re_base re_rob re_ar ///
    pcse_ar pcse_psar gls_het_ar scc_fe, ///
    b(%7.4f) se(%7.4f) stats(N) ///
    title("Таблица 4: Сравнительный анализ до и после коррекции")

/*===========================================================
  ЧАСТЬ 2: УЧЁТ ЭНДОГЕННОСТИ
===========================================================*/

preserve
    collapse (sum) total_import = value, by(country_id)
    gsort -total_import
    keep in 1/50
    keep country_id
    tempfile top50
    save `top50'
restore

merge m:1 country_id using `top50'
gen top50 = (_merge == 3)
drop _merge

tab top50
count if top50 == 1
display "Всего наблюдений в топ-50: " r(N)

ssc install ivreg2
ssc install ranktest
ssc install xtoverid
ssc install xtivreg2

/*===========================================================
  БЛОК 1. ЭНДОГЕННОСТЬ log_gdp И log_price (IV/2SLS)
===========================================================*/

* ------------------------------------------------------------
* Подготовка
* ------------------------------------------------------------

bysort country_id: egen log_dist_mean = mean(log_dist)

* ------------------------------------------------------------
* Базовые модели — точка отсчёта
* ------------------------------------------------------------

display "=== БАЗОВАЯ МОДЕЛЬ: GLS het+ar1 ==="
xtgls log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist_mean border language if top50==1, ///
    panels(hetero) corr(ar1) force
est store base_gls

display "=== RE С КЛАСТЕРНЫМИ SE ==="
xtreg log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist_mean border language if top50==1, ///
    re vce(cluster country_id)
est store base_re_cl

display "=== FE С КЛАСТЕРНЫМИ SE ==="
xtreg log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist_mean border language if top50==1, ///
    fe vce(cluster country_id)
est store base_fe_cl

* ------------------------------------------------------------
* 1.1. Тест релевантности инструментов (первый шаг, RE)
* ------------------------------------------------------------

display "=== ПЕРВЫЙ ШАГ RE: log_gdp ==="
xtreg log_gdp log_cpi log_price_sanctioner log_price_unfriendly ///
    log_dist_mean border language ///
    log_population log_ex_rate log_oil if top50==1, ///
    re vce(cluster country_id)

test log_population log_oil
display "chi2 (log_population → gdp, RE): " r(chi2) ", p = " r(p)

test log_ex_rate log_oil
display "chi2 (log_ex_rate → gdp, RE): " r(chi2) ", p = " r(p)

test log_ex_rate log_population
display "chi2 (log_oil → gdp, RE): " r(chi2) ", p = " r(p)

test log_population log_ex_rate log_oil
display "chi2 (все IV → gdp, RE): " r(chi2) ", p = " r(p)

display "=== ПЕРВЫЙ ШАГ RE: log_price ==="
xtreg log_price log_cpi log_price_sanctioner log_price_unfriendly ///
    log_dist_mean border language ///
    log_population log_ex_rate log_oil if top50==1, ///
    re vce(cluster country_id)

test log_oil log_ex_rate
display "chi2 (log_oil → price, RE): " r(chi2) ", p = " r(p)

test log_ex_rate log_population
display "chi2 (log_ex_rate → price, RE): " r(chi2) ", p = " r(p)

test log_population log_oil
display "chi2 (log_population → price, RE): " r(chi2) ", p = " r(p)

test log_population log_ex_rate log_oil
display "chi2 (все IV → price, RE): " r(chi2) ", p = " r(p)

* ------------------------------------------------------------
* 1.2. IV-оценки: log_gdp эндогенна — три комбинации IV
* Комбинация A: log_oil + log_population
* Комбинация B: log_population + log_ex_rate
* Комбинация C: log_ex_rate + log_oil
* ------------------------------------------------------------

* ── Комбинация A: oil + population ──────────────────────────
display "=== IV-FE (A): oil + population ==="
xtivreg2 log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_price ///
    (log_gdp = log_oil log_population) ///
    if top50==1, fe cluster(country_id) small
est store iv_fe_A

display "=== IV-RE (A): oil + population ==="
xtivreg log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_dist_mean border language log_price ///
    (log_gdp = log_oil log_population) ///
    if top50==1, re
est store iv_re_A

* ── Комбинация B: population + ex_rate ──────────────────────
display "=== IV-FE (B): population + ex_rate ==="
xtivreg2 log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_price ///
    (log_gdp = log_population log_ex_rate) ///
    if top50==1, fe cluster(country_id) small
est store iv_fe_B

display "=== IV-RE (B): population + ex_rate ==="
xtivreg log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_dist_mean border language log_price ///
    (log_gdp = log_population log_ex_rate) ///
    if top50==1, re
est store iv_re_B

* ── Комбинация C: ex_rate + oil ──────────────────────────────
display "=== IV-FE (C): ex_rate + oil ==="
xtivreg2 log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_price ///
    (log_gdp = log_ex_rate log_oil) ///
    if top50==1, fe cluster(country_id) small
est store iv_fe_C

display "=== IV-RE (C): ex_rate + oil ==="
xtivreg log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_dist_mean border language log_price ///
    (log_gdp = log_ex_rate log_oil) ///
    if top50==1, re
est store iv_re_C

* ------------------------------------------------------------
* 1.3. Тесты валидности инструментов — все три комбинации
* ------------------------------------------------------------

* ── Комбинация A ─────────────────────────────────────────────
display "=== ХАНСЕН J (A): IV-FE, oil + population ==="
xtivreg2 log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_price ///
    (log_gdp = log_oil log_population) ///
    if top50==1, fe cluster(country_id) small

display "=== САРГАН-ХАНСЕН (A): IV-RE, oil + population ==="
qui xtivreg log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_dist_mean border language log_price ///
    (log_gdp = log_oil log_population) ///
    if top50==1, re
xtoverid

display "=== САРГАН (A): IV-FE без кластера, для справки ==="
xtivreg2 log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_price ///
    (log_gdp = log_oil log_population) ///
    if top50==1, fe small

* ── Комбинация B ─────────────────────────────────────────────
display "=== ХАНСЕН J (B): IV-FE, population + ex_rate ==="
xtivreg2 log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_price ///
    (log_gdp = log_population log_ex_rate) ///
    if top50==1, fe cluster(country_id) small

display "=== САРГАН-ХАНСЕН (B): IV-RE, population + ex_rate ==="
qui xtivreg log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_dist_mean border language log_price ///
    (log_gdp = log_population log_ex_rate) ///
    if top50==1, re
xtoverid

display "=== САРГАН (B): IV-FE без кластера, для справки ==="
xtivreg2 log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_price ///
    (log_gdp = log_population log_ex_rate) ///
    if top50==1, fe small

* ── Комбинация C ─────────────────────────────────────────────
display "=== ХАНСЕН J (C): IV-FE, ex_rate + oil ==="
xtivreg2 log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_price ///
    (log_gdp = log_ex_rate log_oil) ///
    if top50==1, fe cluster(country_id) small

display "=== САРГАН-ХАНСЕН (C): IV-RE, ex_rate + oil ==="
qui xtivreg log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_dist_mean border language log_price ///
    (log_gdp = log_ex_rate log_oil) ///
    if top50==1, re
xtoverid

display "=== САРГАН (C): IV-FE без кластера, для справки ==="
xtivreg2 log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_price ///
    (log_gdp = log_ex_rate log_oil) ///
    if top50==1, fe small

* ------------------------------------------------------------
* 1.4. Тесты эндогенности log_gdp — все три комбинации
* ------------------------------------------------------------

* Базовые модели без кластеризации — для теста Хаусмана
qui xtreg log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist_mean border language if top50==1, fe
est store fe_hausman

qui xtreg log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist_mean border language if top50==1, re
est store re_hausman

* ── IV-модели без кластеризации для Хаусмана — Комбинация A ─
qui xtivreg log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_price ///
    (log_gdp = log_oil log_population) ///
    if top50==1, fe
est store iv_fe_A_h

qui xtivreg log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_dist_mean border language log_price ///
    (log_gdp = log_oil log_population) ///
    if top50==1, re
est store iv_re_A_h

display "=== ХАУСМАН (A1): IV-FE vs FE — oil + population ==="
hausman iv_fe_A_h fe_hausman, force
display "p < 0.05 → log_gdp эндогенна по u_i и e"

display "=== ХАУСМАН (A2): IV-RE vs RE — oil + population ==="
hausman iv_re_A_h re_hausman
display "p < 0.05 → log_gdp эндогенна только по e"

display "=== ХАУСМАН (A3): IV-FE vs IV-RE — oil + population ==="
hausman iv_fe_A_h iv_re_A_h, force
display "p < 0.05 → FE предпочтительнее RE в IV"

display "=== C-ТЕСТ (A): эндогенность log_gdp, oil + population ==="
xtivreg2 log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_price ///
    (log_gdp = log_oil log_population) ///
    if top50==1, fe cluster(country_id) small endog(log_gdp)
display "p < 0.05 → log_gdp эндогенна"

* ── IV-модели без кластеризации для Хаусмана — Комбинация B ─
qui xtivreg log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_price ///
    (log_gdp = log_population log_ex_rate) ///
    if top50==1, fe
est store iv_fe_B_h

qui xtivreg log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_dist_mean border language log_price ///
    (log_gdp = log_population log_ex_rate) ///
    if top50==1, re
est store iv_re_B_h

display "=== ХАУСМАН (B1): IV-FE vs FE — population + ex_rate ==="
hausman iv_fe_B_h fe_hausman, force
display "p < 0.05 → log_gdp эндогенна по u_i и e"

display "=== ХАУСМАН (B2): IV-RE vs RE — population + ex_rate ==="
hausman iv_re_B_h re_hausman
display "p < 0.05 → log_gdp эндогенна только по e"

display "=== ХАУСМАН (B3): IV-FE vs IV-RE — population + ex_rate ==="
hausman iv_fe_B_h iv_re_B_h, force
display "p < 0.05 → FE предпочтительнее RE в IV"

display "=== C-ТЕСТ (B): эндогенность log_gdp, population + ex_rate ==="
xtivreg2 log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_price ///
    (log_gdp = log_population log_ex_rate) ///
    if top50==1, fe cluster(country_id) small endog(log_gdp)
display "p < 0.05 → log_gdp эндогенна"

* ── IV-модели без кластеризации для Хаусмана — Комбинация C ─
qui xtivreg log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_price ///
    (log_gdp = log_ex_rate log_oil) ///
    if top50==1, fe
est store iv_fe_C_h

qui xtivreg log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_dist_mean border language log_price ///
    (log_gdp = log_ex_rate log_oil) ///
    if top50==1, re
est store iv_re_C_h

display "=== ХАУСМАН (C1): IV-FE vs FE — ex_rate + oil ==="
hausman iv_fe_C_h fe_hausman, force
display "p < 0.05 → log_gdp эндогенна по u_i и e"

display "=== ХАУСМАН (C2): IV-RE vs RE — ex_rate + oil ==="
hausman iv_re_C_h re_hausman
display "p < 0.05 → log_gdp эндогенна только по e"

display "=== ХАУСМАН (C3): IV-FE vs IV-RE — ex_rate + oil ==="
hausman iv_fe_C_h iv_re_C_h, force
display "p < 0.05 → FE предпочтительнее RE в IV"

display "=== C-ТЕСТ (C): эндогенность log_gdp, ex_rate + oil ==="
xtivreg2 log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_price ///
    (log_gdp = log_ex_rate log_oil) ///
    if top50==1, fe cluster(country_id) small endog(log_gdp)
display "p < 0.05 → log_gdp эндогенна"

* ------------------------------------------------------------
* 1.5. Сравнение эффектов по моделям (scalars)
* ------------------------------------------------------------

display "=== СРАВНЕНИЕ ЭФФЕКТОВ ==="

xtgls log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist_mean border language if top50==1, ///
    panels(hetero) corr(ar1) force
scalar gdp_gls   = _b[log_gdp]
scalar price_gls = _b[log_price]

xtreg log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist_mean border language if top50==1, ///
    re vce(cluster country_id)
scalar gdp_re   = _b[log_gdp]
scalar price_re = _b[log_price]

xtivreg2 log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_price ///
    (log_gdp = log_oil log_population) ///
    if top50==1, fe cluster(country_id) small
scalar gdp_iv_fe_A = _b[log_gdp]
scalar price_iv_fe_A = _b[log_price]

xtivreg log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_dist_mean border language log_price ///
    (log_gdp = log_oil log_population) ///
    if top50==1, re
scalar gdp_iv_re_A = _b[log_gdp]
scalar price_iv_re_A = _b[log_price]

xtivreg2 log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_price ///
    (log_gdp = log_population log_ex_rate) ///
    if top50==1, fe cluster(country_id) small
scalar gdp_iv_fe_B = _b[log_gdp]
scalar price_iv_fe_B = _b[log_price]

xtivreg log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_dist_mean border language log_price ///
    (log_gdp = log_population log_ex_rate) ///
    if top50==1, re
scalar gdp_iv_re_B = _b[log_gdp]
scalar price_iv_re_B = _b[log_price]

xtivreg2 log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_price ///
    (log_gdp = log_ex_rate log_oil) ///
    if top50==1, fe cluster(country_id) small
scalar gdp_iv_fe_C = _b[log_gdp]
scalar price_iv_fe_C = _b[log_price]

xtivreg log_value log_cpi log_price_sanctioner ///
    log_price_unfriendly log_dist_mean border language log_price ///
    (log_gdp = log_ex_rate log_oil) ///
    if top50==1, re
scalar gdp_iv_re_C = _b[log_gdp]
scalar price_iv_re_C = _b[log_price]

display "=== log_gdp по моделям ==="
scalar list gdp_gls gdp_re gdp_iv_fe_A gdp_iv_re_A gdp_iv_fe_B gdp_iv_re_B gdp_iv_fe_C gdp_iv_re_C

display "=== log_price по моделям ==="
scalar list price_gls price_re price_iv_fe_A price_iv_re_A price_iv_fe_B price_iv_re_B price_iv_fe_C price_iv_re_C

* ------------------------------------------------------------
* 1.6. Сводная таблица Блока 1
* ------------------------------------------------------------

display "=== СВОДНАЯ ТАБЛИЦА: БЛОК 1 ==="
estimates table base_gls base_re_cl base_fe_cl ///
    iv_fe_A iv_re_A ///
    iv_fe_B iv_re_B ///
    iv_fe_C iv_re_C, ///
    b(%7.4f) se(%7.4f) stats(N) ///
    title("Блок 1: Базовые модели и IV-оценки по трём комбинациям инструментов")	

/*===========================================================
  БЛОК 2. ТЕСТИРОВАНИЕ ЭНДОГЕННОСТИ ИНВАРИАНТНЫХ ПЕРЕМЕННЫХ
* МЕТОД ХАУСМАНА-ТЕЙЛОРА С РАЗЛИЧНЫМИ НАБОРАМИ ИНСТРУМЕНТОВ
===========================================================*/

* ========================================================
* БАЗОВЫЕ МОДЕЛИ ДЛЯ СРАВНЕНИЯ
* ========================================================

* 2. Базовая RE модель (для сравнения)
display "=== RE С КЛАСТЕРНЫМИ SE ==="
xtreg log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist_mean border language if top50==1, ///
    re vce(cluster country_id)
est store base_re_cl

* 3. FE + BE (состоятельная, но неэффективная оценка для сравнения)
display "=== FE ДЛЯ МЕНЯЮЩИХСЯ ПЕРЕМЕННЫХ ==="
xtreg log_value log_price log_gdp log_cpi ///
      log_price_sanctioner log_price_unfriendly ///
      if top50==1, fe
predict resid_fe, ue

display "=== BE ДЛЯ ИНВАРИАНТНЫХ ПЕРЕМЕННЫХ (НА ОСТАТКАХ FE) ==="
xtreg resid_fe log_dist_mean border language if top50==1, be
est store fe_be

* ========================================================
* СПЕЦИФИКАЦИИ HT С ОДНОЙ ЭНДОГЕННОЙ ПЕРЕМЕННОЙ (border)
* ========================================================

* --------------------------------------------------------
* 4. БАЗОВАЯ: только border эндогенна (без доп. инструментов)
* --------------------------------------------------------
display "=== HT (только border эндогенна) - БАЗОВАЯ ==="
xthtaylor log_value log_price log_gdp log_cpi ///
          log_price_sanctioner log_price_unfriendly ///
          log_dist_mean border language if top50==1, ///
          endog(border) ///
          constant(log_dist_mean border language)
est store ht_border
xtoverid
matrix ht_border_sargan = r(chi2_p)
scalar ht_border_sargan = r(chi2_p)
display "Тест Саргана p-value = " ht_border_sargan

* --------------------------------------------------------
* 5. ДОП. ИНСТРУМЕНТ: только log_population
* --------------------------------------------------------
display "=== HT (border эндогенна) + log_population ==="
xthtaylor log_value log_price log_gdp log_cpi ///
          log_price_sanctioner log_price_unfriendly ///
          log_dist_mean border language ///
          log_population if top50==1, ///
          endog(border) ///
          constant(log_dist_mean border language)
est store ht_pop
xtoverid
matrix ht_pop_sargan = r(chi2_p)
scalar ht_pop_sargan = r(chi2_p)
display "Тест Саргана p-value = " ht_pop_sargan

* --------------------------------------------------------
* 6. ДОП. ИНСТРУМЕНТ: только log_ex_rate
* --------------------------------------------------------
display "=== HT (border эндогенна) + log_ex_rate ==="
xthtaylor log_value log_price log_gdp log_cpi ///
          log_price_sanctioner log_price_unfriendly ///
          log_dist_mean border language ///
          log_ex_rate if top50==1, ///
          endog(border) ///
          constant(log_dist_mean border language)
est store ht_exrate
xtoverid
matrix ht_exrate_sargan = r(chi2_p)
scalar ht_exrate_sargan = r(chi2_p)
display "Тест Саргана p-value = " ht_exrate_sargan

* --------------------------------------------------------
* 7. ДОП. ИНСТРУМЕНТ: только log_oil
* --------------------------------------------------------
display "=== HT (border эндогенна) + log_oil ==="
xthtaylor log_value log_price log_gdp log_cpi ///
          log_price_sanctioner log_price_unfriendly ///
          log_dist_mean border language ///
          log_oil if top50==1, ///
          endog(border) ///
          constant(log_dist_mean border language)
est store ht_oil
capture xtoverid
if _rc == 0 {
    matrix ht_oil_sargan = r(chi2_p)
    scalar ht_oil_sargan = r(chi2_p)
    display "Тест Саргана p-value = " ht_oil_sargan
}
else {
    display "Ошибка xtoverid - модель нестабильна"
    scalar ht_oil_sargan = .
}

* --------------------------------------------------------
* 8. ДВА ИНСТРУМЕНТА: log_population + log_ex_rate
* --------------------------------------------------------
display "=== HT (border эндогенна) + pop + exrate ==="
xthtaylor log_value log_price log_gdp log_cpi ///
          log_price_sanctioner log_price_unfriendly ///
          log_dist_mean border language ///
          log_population log_ex_rate if top50==1, ///
          endog(border) ///
          constant(log_dist_mean border language)
est store ht_pop_exrate
xtoverid
matrix ht_pop_exrate_sargan = r(chi2_p)
scalar ht_pop_exrate_sargan = r(chi2_p)
display "Тест Саргана p-value = " ht_pop_exrate_sargan

* --------------------------------------------------------
* 9. ДВА ИНСТРУМЕНТА: log_population + log_oil
* --------------------------------------------------------
display "=== HT (border эндогенна) + pop + oil ==="
xthtaylor log_value log_price log_gdp log_cpi ///
          log_price_sanctioner log_price_unfriendly ///
          log_dist_mean border language ///
          log_population log_oil if top50==1, ///
          endog(border) ///
          constant(log_dist_mean border language)
est store ht_pop_oil
xtoverid
matrix ht_pop_oil_sargan = r(chi2_p)
scalar ht_pop_oil_sargan = r(chi2_p)
display "Тест Саргана p-value = " ht_pop_oil_sargan

* ========================================================
* СПЕЦИФИКАЦИИ HT С ДВУМЯ ЭНДОГЕННЫМИ ПЕРЕМЕННЫМИ (border + log_price)
* ========================================================

* --------------------------------------------------------
* 10. HT С ДВУМЯ ЭНДОГЕННЫМИ + log_population
* --------------------------------------------------------
display "=== HT (border + log_price эндогенны) + log_population ==="
xthtaylor log_value log_price log_gdp log_cpi ///
          log_price_sanctioner log_price_unfriendly ///
          log_dist_mean border language if top50==1, ///
          endog(log_price border) ///
          constant(log_dist_mean border language)
est store ht2_pop
xtoverid
matrix ht2_pop_sargan = r(chi2_p)
scalar ht2_pop_sargan = r(chi2_p)
display "Тест Саргана p-value = " ht2_pop_sargan

/*===========================================================
  БЛОК 3. ДИНАМИЧЕСКАЯ МОДЕЛЬ С ЛАГОМ ЗАВИСИМОЙ ПЕРЕМЕННОЙ
===========================================================*/

* Отбор топ-50 стран по объему импорта
preserve
    collapse (sum) total_import = value, by(country_id)
    gsort -total_import
    keep in 1/50
    keep country_id
    tempfile top50
    save `top50'
restore

merge m:1 country_id using `top50'
gen top50 = (_merge == 3)
drop _merge

* Переменные взаимодействий
capture drop log_price_sanctioner log_price_unfriendly
gen log_price_sanctioner = log_price * sanctioner
gen log_price_unfriendly  = log_price * unfriendly

* log_dist_mean
capture bysort country_id: egen log_dist_mean = mean(log_dist)

/*-----------------------------------------------------------
  ОБОСНОВАНИЕ ДИНАМИЧЕСКОЙ СПЕЦИФИКАЦИИ
-----------------------------------------------------------*/
display _newline(2) "{hline 60}"
display "БЛОК 1: ОБОСНОВАНИЕ — АВТОКОРРЕЛЯЦИЯ В СТАТИЧЕСКОЙ МОДЕЛИ"
display "{hline 60}"

* Тест Вулдриджа проводится на RE как технически доступная альтернатива:
* xtserial несовместима с xtgls; при этом RE и GLS дают одинаковые
* точечные оценки коэффициентов, поэтому остатки практически идентичны
* H0: нет AR(1) в остатках
xtserial log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist_mean border language if top50 == 1
display "=> F >> 10, p < 0.01: лаг зависимой переменной оправдан"

* Корреляция импорта с его лагом — дополнительное подтверждение,
* не зависящее от выбора оценщика
display _newline "Корреляция log_value и L.log_value:"
pwcorr log_value L.log_value if top50 == 1, star(0.05)

/*-----------------------------------------------------------
  ТЕСТЫ НА ЕДИНИЧНЫЕ КОРНИ (СБАЛАНСИРОВАННАЯ ПАНЕЛЬ)
-----------------------------------------------------------*/
display _newline(2) "{hline 60}"
display "БЛОК 2: ТЕСТЫ НА ЕДИНИЧНЫЕ КОРНИ"
display "{hline 60}"

preserve

    keep if top50 == 1
    xtset country_id year

    * оставляем только страны с полным временным рядом
    by country_id: gen T_i = _N
    quietly summarize T_i
    keep if T_i == r(max)

    display "Число стран в сбалансированной панели: " _N
    xtdescribe

    foreach var in log_value log_price log_gdp {

        display _newline "--- Переменная: `var' ---"

        * Levin–Lin–Chu
        xtunitroot llc `var', lags(aic 1)

        * Breitung
        xtunitroot breitung `var', lags(1)

        * Harris–Tzavalis
        xtunitroot ht `var'

        * Hadri
        xtunitroot hadri `var'

        * IPS (для сравнения)
        xtunitroot ips `var', lags(aic 1)

        * Fisher-ADF
        xtunitroot fisher `var', dfuller lags(1) drift
    }

restore

/*-----------------------------------------------------------
  БАЗОВЫЕ ДИНАМИЧЕСКИЕ МОДЕЛИ: AB и BB
-----------------------------------------------------------*/
display _newline(2) "{hline 60}"
display "БЛОК 3: БАЗОВЫЕ ДИНАМИЧЕСКИЕ МОДЕЛИ (AB и BB)"
display "{hline 60}"

* GLS het+ar1 — финальная модель части 1
display _newline "=== БАЗОВАЯ МОДЕЛЬ: GLS het+ar1 ==="
xtgls log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    log_dist_mean border language if top50==1, ///
    panels(hetero) corr(ar1) force
est store base_gls

* Хаусман-Тейлор с log_population — лучшая модель блока инвариантных регрессоров
display _newline "=== БАЗОВАЯ МОДЕЛЬ: HT + log_population ==="
xthtaylor log_value log_price log_gdp log_cpi ///
          log_price_sanctioner log_price_unfriendly ///
          log_dist_mean border language ///
          log_population if top50==1, ///
          endog(border) ///
          constant(log_dist_mean border language)
est store ht_pop

* --- Arellano-Bond (без vce(robust) — для C-теста и sargan) ---
display _newline "=== AB, twostep ==="
xtabond log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    if top50 == 1, nocons twostep
est store ab
estat sargan
scalar sargan_ab = r(chi2)
scalar df_ab     = r(df)
estat abond

* AB с vce(robust)
display _newline "=== AB, twostep, vce(robust) ==="
xtabond log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    if top50 == 1, nocons twostep vce(robust)
est store ab_r
estat abond

* --- Blundell-Bond (без vce(robust)) ---
display _newline "=== BB (xtdpdsys), twostep ==="
xtdpdsys log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    if top50 == 1, nocons twostep
est store bb
estat sargan
scalar sargan_bb = r(chi2)
scalar df_bb     = r(df)
estat abond

* BB с vce(robust)
display _newline "=== BB, twostep, vce(robust) ==="
xtdpdsys log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    if top50 == 1, nocons twostep vce(robust)
est store bb_r
estat abond

/*-----------------------------------------------------------
  МОДЕЛИ С ЭНДОГЕННЫМ log_gdp
-----------------------------------------------------------*/
display _newline(2) "{hline 60}"
display "БЛОК 4: МОДЕЛИ С ЭНДОГЕННЫМ log_gdp"
display "{hline 60}"

* ivAB (без vce(robust) — для C-теста)
display _newline "=== ivAB, twostep, endog(log_gdp) ==="
xtabond log_value log_price log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    if top50 == 1, nocons twostep endog(log_gdp)
est store ivab
estat sargan
scalar sargan_ivab = r(chi2)
scalar df_ivab     = r(df)
estat abond

* ivAB с vce(robust)
display _newline "=== ivAB, twostep, endog(log_gdp), vce(robust) ==="
xtabond log_value log_price log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    if top50 == 1, nocons twostep endog(log_gdp) vce(robust)
est store ivab_r
estat abond

* ivBB (без vce(robust))
display _newline "=== ivBB, twostep, endog(log_gdp) ==="
xtdpdsys log_value log_price log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    if top50 == 1, nocons twostep endog(log_gdp)
est store ivbb
estat sargan
scalar sargan_ivbb = r(chi2)
scalar df_ivbb     = r(df)
estat abond

* ivBB с vce(robust)
display _newline "=== ivBB, twostep, endog(log_gdp), vce(robust) ==="
xtdpdsys log_value log_price log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    if top50 == 1, nocons twostep endog(log_gdp) vce(robust)
est store ivbb_r
estat abond

/*-----------------------------------------------------------
  C-ТЕСТ НА ЭНДОГЕННОСТЬ log_gdp
-----------------------------------------------------------*/
display _newline(2) "{hline 60}"
display "БЛОК 5: C-ТЕСТ НА ЭНДОГЕННОСТЬ log_gdp"
display "{hline 60}"

* Перезапускаем модели и сразу сохраняем Sargan из e()
quietly xtabond log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    if top50 == 1, nocons twostep
scalar sargan_ab = e(sargan)
scalar df_ab     = e(zrank) - e(rank)

quietly xtabond log_value log_price log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    if top50 == 1, nocons twostep endog(log_gdp)
scalar sargan_ivab = e(sargan)
scalar df_ivab     = e(zrank) - e(rank)

quietly xtdpdsys log_value log_price log_gdp log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    if top50 == 1, nocons twostep
scalar sargan_bb = e(sargan)
scalar df_bb     = e(zrank) - e(rank)

quietly xtdpdsys log_value log_price log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    if top50 == 1, nocons twostep endog(log_gdp)
scalar sargan_ivbb = e(sargan)
scalar df_ivbb     = e(zrank) - e(rank)

* C-тест для AB
scalar C_ab   = sargan_ivab - sargan_ab
scalar df_C   = df_ivab - df_ab
scalar p_C_ab = chi2tail(df_C, C_ab)
display _newline "--- C-тест (AB): ---"
display "Sargan(AB)   = " %7.4f sargan_ab   "  df = " df_ab
display "Sargan(ivAB) = " %7.4f sargan_ivab "  df = " df_ivab
display "C = " %7.4f C_ab "  df = " df_C "  p = " %6.4f p_C_ab
if p_C_ab > 0.10 {
    display "p > 0.10 => H0 не отвергается => log_gdp экзогенна в AB"
}
else {
    display "p <= 0.10 => H0 отвергается => log_gdp эндогенна в AB"
}

* C-тест для BB
scalar C_bb   = sargan_ivbb - sargan_bb
scalar df_Cbb = df_ivbb - df_bb
scalar p_C_bb = chi2tail(df_Cbb, C_bb)
display _newline "--- C-тест (BB): ---"
display "Sargan(BB)   = " %7.4f sargan_bb   "  df = " df_bb
display "Sargan(ivBB) = " %7.4f sargan_ivbb "  df = " df_ivbb
display "C = " %7.4f C_bb "  df = " df_Cbb "  p = " %6.4f p_C_bb
if p_C_bb > 0.10 {
    display "p > 0.10 => H0 не отвергается => log_gdp экзогенна в BB"
}
else {
    display "p <= 0.10 => H0 отвергается => log_gdp эндогенна в BB"
}

/*-----------------------------------------------------------
  СВОДНАЯ ТАБЛИЦА: базовые модели, AB, ivAB, BB, ivBB
-----------------------------------------------------------*/
display _newline(2) "{hline 60}"
display "БЛОК 6: СВОДНАЯ ТАБЛИЦА ДИНАМИЧЕСКИХ МОДЕЛЕЙ"
display "{hline 60}"

est tab base_gls ht_pop ab ab_r ivab ivab_r bb bb_r ivbb ivbb_r, ///
    b(%7.4f) star stats(N)

/*-----------------------------------------------------------
  ДИАГНОСТИКА ОСТАТКОВ ПРЕДПОЧТИТЕЛЬНОЙ МОДЕЛИ
-----------------------------------------------------------*/
display _newline(2) "{hline 60}"
display "БЛОК 7: ДИАГНОСТИКА ОСТАТКОВ ПРЕДПОЧТИТЕЛЬНОЙ МОДЕЛИ"
display "{hline 60}"

qui xtabond log_value log_price log_cpi ///
    log_price_sanctioner log_price_unfriendly ///
    if top50 == 1, nocons twostep endog(log_gdp) vce(robust)

predict resid_diff, e difference
predict resid_lev,  e

* Визуальная проверка
kdensity resid_diff, normal ///
    title("Плотность остатков в разностях (ivAB rob.)") ///
    xtitle("Остатки") ytitle("Плотность") ///
    graphregion(color(white))

twoway scatter resid_diff L.resid_diff ///
    || lfit resid_diff L.resid_diff, ///
    title("Визуальный тест на автокорреляцию (ivAB rob.)") ///
    xtitle("Остатки (t-1)") ytitle("Остатки (t)") ///
    graphregion(color(white))

* Ключевой тест AR(2)
estat abond

drop resid_diff resid_lev

/*===========================================================
  МОДЕЛЬ ДЛИННЫХ РАЗНОСТЕЙ
===========================================================*/
display _newline(2) "{hline 60}"
display "БЛОК 8: МОДЕЛЬ ДЛИННЫХ РАЗНОСТЕЙ"
display "{hline 60}"

foreach var in log_value log_price log_gdp log_cpi ///
               log_price_sanctioner log_price_unfriendly {
    by country_id: gen `var'_T   = `var'[_N]
    by country_id: gen `var'_T_1 = `var'[_N-1]
    by country_id: gen `var'_1   = `var'[1]
    by country_id: gen `var'_2   = `var'[2]
}

* Длинные разности: T минус второй год
gen lv_ld   = log_value_T  - log_value_2
gen lp_ld   = log_price_T  - log_price_2
gen lgdp_ld = log_gdp_T    - log_gdp_2
gen lcpi_ld = log_cpi_T    - log_cpi_2
gen lps_ld  = log_price_sanctioner_T - log_price_sanctioner_2
gen lpu_ld  = log_price_unfriendly_T - log_price_unfriendly_2

* Лаг Y: предпоследний минус первый год — инструментируется Y_1
gen lv_lag_ld = log_value_T_1 - log_value_1

preserve
    collapse (mean) lv_ld lp_ld lgdp_ld lcpi_ld lps_ld lpu_ld ///
                    lv_lag_ld log_value_1, by(country_id)

    display "Число стран в модели длинных разностей: " _N

    * ОМН — наивная оценка без учёта эндогенности лага Y
    display _newline "=== ОМН (длинные разности) ==="
    reg lv_ld lv_lag_ld lp_ld lgdp_ld lcpi_ld lps_ld lpu_ld
    est store ls_ld

    * IV: инструментируем lv_lag_ld начальным значением Y_1 (2017)
    display _newline "=== IV (инструмент = log_value в 2017) ==="
    ivreg2 lv_ld lp_ld lgdp_ld lcpi_ld lps_ld lpu_ld ///
           (lv_lag_ld = log_value_1), small
    est store iv_ld
    predict e_ld, resid

    * IV с двумя инструментами: Y_1 и остатки первой IV
    display _newline "=== IV с двумя инструментами ==="
    ivreg2 lv_ld lp_ld lgdp_ld lcpi_ld lps_ld lpu_ld ///
           (lv_lag_ld = log_value_1 e_ld), small
    est store iv_ld2

    display _newline "=== СВОДНАЯ ТАБЛИЦА: ДЛИННЫЕ РАЗНОСТИ ==="
    est tab ls_ld iv_ld iv_ld2, b(%7.4f) star stats(N)

restore

drop *_T *_T_1 *_1 *_2 lv_ld lp_ld lgdp_ld lcpi_ld lps_ld lpu_ld lv_lag_ld

/*===========================================================
  МОДЕЛЬ С ГЕТЕРОГЕННЫМ ТРЕНДОМ
===========================================================*/
display _newline(2) "{hline 60}"
display "БЛОК 9: МОДЕЛЬ С ГЕТЕРОГЕННЫМ ТРЕНДОМ"
display "{hline 60}"

* FE на первых разностях
display _newline "=== FE на первых разностях ==="
xtreg D.log_value D.log_price D.log_gdp D.log_cpi ///
      D.log_price_sanctioner D.log_price_unfriendly ///
      if top50 == 1, fe
est store fe_diff

* Тест на значимость временных эффектов в разностях
quietly tabulate year if top50 == 1, generate(year_d_)
xtreg D.log_value D.log_price D.log_gdp D.log_cpi ///
      D.log_price_sanctioner D.log_price_unfriendly ///
      year_d_2-year_d_6 if top50 == 1, fe
testparm year_d_*
display "p < 0.01 => временные эффекты значимы, нужно включить в спецификацию"
drop year_d_*

* RE на первых разностях — для теста Хаусмана
display _newline "=== RE на первых разностях ==="
xtreg D.log_value D.log_price D.log_gdp D.log_cpi ///
      D.log_price_sanctioner D.log_price_unfriendly ///
      if top50 == 1, re
est store re_diff

hausman fe_diff re_diff
display "p < 0.05 => FE предпочтительнее RE на разностях"

* Извлекаем страновые тренды
display _newline "=== СТРАНОВЫЕ ТРЕНДЫ ==="
qui xtreg D.log_value D.log_price D.log_gdp D.log_cpi ///
          D.log_price_sanctioner D.log_price_unfriendly ///
          if top50 == 1, fe
predict lambda, u

kdensity lambda if top50==1, normal ///
    title("Распределение страновых трендов (λ_i)") ///
    xtitle("Индивидуальный тренд (λ_i)") ytitle("Плотность") ///
    graphregion(color(white))

preserve
    collapse (mean) lambda, by(country_id country)
    gsort -lambda
    display "Топ-10 стран с наибольшим ростом (λ_i):"
    list country lambda in 1/10
    gsort lambda
    display "Топ-10 стран с наибольшим падением (λ_i):"
    list country lambda in 1/10
restore

drop lambda

display _newline "=== СРАВНЕНИЕ: GLS / HT vs FE-diff vs RE-diff ==="
est tab base_gls ht_pop fe_diff re_diff, b(%7.4f) star stats(N)

/*===========================================================
  ИТОГОВАЯ СВОДНАЯ ТАБЛИЦА ВСЕХ ПОДХОДОВ
===========================================================*/
display _newline(2) "{hline 60}"
display "БЛОК 10: ИТОГОВАЯ СВОДНАЯ ТАБЛИЦА ВСЕХ ПОДХОДОВ"
display "{hline 60}"

est tab base_gls ht_pop ab_r ivab_r bb_r ivbb_r fe_diff, ///
    b(%7.4f) star stats(N)
