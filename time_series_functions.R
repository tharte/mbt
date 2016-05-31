`assert`<- stopifnot
`.init.proj`<- function() {
	options(width=220)

    library(zoo)
    library(openxlsx)
    library(grid)
    library(gridExtra)
    library(ggplot2)
    library(scales)
    library(ggthemes)
    library(stringr)
    library(lubridate)
    library(readr)
    library(knitr)
    library(reshape2)
    library(magrittr)
    library(lazyeval)
    library(dplyr)
    library(rstan)

	invisible()
}
`bind_to_env`<- function(object, envir) {
    assert(is.environment(envir))

    nm<- as.character(substitute(object))
    assign(nm, object, env=envir)
    if (is.function(object))
		environment(envir[[nm]])<- as.environment(envir)

    invisible()
}
`get_data_dir`<- function() {
    file.path("data")
}
`read_time_series_info`<- function() {
    filename<- file.path(get_data_dir(), "info.xlsx")

    read.xlsx(filename, sheet=1) %>%
        mutate(header=ifelse(header==1, TRUE, FALSE)) %>%
        filter(!is.na(`Name`))
}
`read_time_series_data`<- function() {
    # source: ftp://ftp.wiley.com/public/sci_tech_med/time_series/time_series.zip
    # "Introduction to Time Series Analysis and Forecasting"
    #
    #    Douglas C. Montgomery, Cheryl L. Jennings, Murat Kulahci
    #    ISBN: 978-0-471-65397-4
    #    472 pages
    #    March 2008, ©2008
    #
    # http://www.wiley.com/WileyCDA/WileyTitle/productCd-0471653977.html

    filename<- file.path(get_data_dir(), "Time Series and Forecasting Appendix B Tables.xlsx")
    `read_tsaf`<- function() {
        tsaf<- "\"Introduction to Time Series Analysis and Forecasting\", Montgomery, Jennings & Kulahci (2008)"
        info %<>% filter(Source==tsaf)
        sheets<- info %>% select(Sheet) %>% .[[1]]

        out<- lapply(sheets, function(sheet) {
            cat(sprintf("reading '%s'\n", sheet))
            read.xlsx(
                filename,
                sheet = sheet,
                colNames = info %>% filter(Sheet==sheet) %>% select(header) %>% .[[1]],
                startRow = 1 + info %>% filter(Sheet==sheet) %>% select(skip) %>% .[[1]],
                check.names = FALSE,
                detectDates = TRUE
            )
        })
        names(out)<- sheets

        out
    }

    info<- read_time_series_info()
    ##  FIXME: replace the evil with closure
    assign("info", info, .GlobalEnv)

    tabs<- read_tsaf()

    `make_adjustments`<- function(sheet) {
        origin<- "1899-12-30"
        `adjust`<- function(sheet) tabs[[sheet]]
        `stack`<-  function(tab, indices)
            do.call("rbind", lapply(indices, function(index) tab[, index]))

        switch(sheet,
            `B.1-10YTCM` = {
                `adjust`<- function(sheet) {
                    tab<- tabs[[sheet]]

                    cols<- which(colnames(tabs[[sheet]])=="Month")
                    for (col in cols)
                        tab[, col]<- as.Date(tab[, col], origin=origin)

                    tab<- stack(tab, list(1:2, 3:4, 5:6, 7:8))
                    colnames(tab)<- c("Month", "Rate (%)")

                    tab %>% filter(!is.na(`Rate (%)`))
                }
            },
            `B.2-PHAR` = {
                `adjust`<- function(sheet) {
                    tab<- tabs[[sheet]]

                    tab<- stack(tab, list(1:2, 3:4, 5:6, 7:8))
                    colnames(tab)<- c("Week", "Sales (thousands)")

                    tab %>% filter(!is.na(Week))
                }
            },
            `B.3-VISC` = {
                `adjust`<- function(sheet) {
                    tab<- tabs[[sheet]]

                    tab<- stack(tab, list(1:2, 3:4, 5:6))
                    colnames(tab)<- c("Time Period", "Reading")

                    tab %>% filter(!is.na(`Time Period`))
                }
            },
            `B.4-BLUE` = {
                `adjust`<- function(sheet) {
                    tab<- tabs[[sheet]]

                    colnames(tab)<- c("Year", "Production (thousands)")

                    tab %>% filter(!is.na(`Year`))
                }
            },
            `B.5-BEV` = {
                `adjust`<- function(sheet) {
                    tab<- tabs[[sheet]]

                    cols<- which(colnames(tabs[[sheet]])=="Month")
                    for (col in cols)
                        tab[, col]<- as.Date(tab[, col], origin=origin)

                    tab<- stack(tab, list(1:2, 3:4, 5:6, 7:8))
                    colnames(tab)<- c("Month", "USD (millions)")

                    tab %>% filter(!is.na(`Month`))
                }
            },
            `B.6-GSAT-CO2` = {
                `adjust`<- function(sheet) {
                    tab<- tabs[[sheet]]

                    tab<- stack(tab, list(1:3, 4:6, 7:9))
                    colnames(tab)<- c("Year", "Surface Temp (C)", "CO2 (ppmv)")

                    tab %>% filter(!is.na(`Year`))
                }
            },
            `B.7-WFMI` = {
                `adjust`<- function(sheet) {
                    tab<- tabs[[sheet]]

                    cols<- which(colnames(tabs[[sheet]])=="Date")
                    for (col in cols)
                        tab[, col]<- as.Date(tab[, col], origin=origin)

                    tab<- stack(tab, list(1:2, 3:4, 5:6, 7:8, 9:10))
                    colnames(tab)<- c("Date", "Price (USD)")

                    tab %>% filter(!is.na(`Date`))
                }
            },
            `B.8-UNEMP` = {
                `adjust`<- function(sheet) {
                    tab<- tabs[[sheet]]

                    cols<- which(colnames(tabs[[sheet]])=="Month")
                    for (col in cols)
                        tab[, col]<- as.Date(tab[, col], origin=origin)

                    tab<- stack(tab, list(1:2, 3:4, 5:6, 7:8, 9:10, 11:12))
                    colnames(tab)<- c("Month", "Unemployment Rate (%)")

                    tab %>% filter(!is.na(`Month`))
                }
            },
            `B.9-SUNSPOT` = {
                `adjust`<- function(sheet) {
                    tab<- tabs[[sheet]]

                    tab<- stack(tab, list(1:2, 3:4, 5:6, 7:8, 9:10))
                    colnames(tab)<- c("Year", "Sunspot Number")

                    tab %>% filter(!is.na(`Year`))
                }
            },
            `B.10-FLOWN` = {
                `adjust`<- function(sheet) {
                    tab<- tabs[[sheet]]

                    cols<- which(colnames(tabs[[sheet]])=="Month")
                    for (col in cols)
                        tab[, col]<- as.Date(tab[, col], origin=origin)

                    colnames(tab)<- c("Month", "Miles Flown (millions)")

                    tab %>% filter(!is.na(`Month`))
                }
            },
            `B.11-CHAMP` = {
                `adjust`<- function(sheet) {
                    tab<- tabs[[sheet]]

                    cols<- which(colnames(tabs[[sheet]])=="Month")
                    for (col in cols)
                        tab[, col]<- as.Date(tab[, col], origin=origin)

                    colnames(tab)<- c("Month", "Sales (thousands)")

                    tab %>% filter(!is.na(`Month`))
                }
            },
            `B.12-YIELD` = {
                `adjust`<- function(sheet) {
                    tab<- tabs[[sheet]]

                    colnames(tab)<- c("Hour", "Yield (%)", "Temp (F)")

                    tab %>% filter(!is.na(`Hour`))
                }
            },
            `B.13-IC-YO` = {
                `adjust`<- function(sheet) {
                    tab<- tabs[[sheet]]

                    colnames(tab)<- c(
                        "Year",
                        "Ice Cream (gallons, thousands)",
                        "Frozen Yogurt (gallons, thousands)"
                    )
                    tab[, 3]<- as.numeric(gsub("-", "", tab[, 3]))

                    tab %>% filter(!is.na(`Year`))
                }
            },
            `B.14-MAUNA` = {
                `adjust`<- function(sheet) {
                    tab<- tabs[[sheet]]

                    colnames(tab)<- c("Year", "CO2 (ppmv)")

                    tab %>% filter(!is.na(`Year`))
                }
            },
            `B.15-CRIME` = {
                `adjust`<- function(sheet) {
                    tab<- tabs[[sheet]]

                    ##  remove extraneous note from end:
                    tab<- tab[1:(nrow(tab)-2), ]

                    colnames(tab)<- c("Year", "Crime Rate (per 100k)")

                    tab %>% filter(!is.na(`Year`))
                }
            },
            `B.16-GDP` = {
                `adjust`<- function(sheet) {
                    tab<- tabs[[sheet]]

                    colnames(tab)<- c(
                        "Year",
                        "GDP (USD current, billions)",
                        "GDP (USD real, billions)"
                    )

                    tab %>% filter(!is.na(`Year`))
                }
            },
            `B.17-ENERGY` = {
                `adjust`<- function(sheet) {
                    tab<- tabs[[sheet]]

                    colnames(tab)<- c("Year", "BTU (billions)")

                    tab %>% filter(!is.na(`Year`))
                }
            },
            `B.18-COAL` = {
                `adjust`<- function(sheet) {
                    tab<- tabs[[sheet]]

                    colnames(tab)<- c("Year", "Coal (short tons, thousands)")

                    tab %>% filter(!is.na(`Year`))
                }
            },
            `B.19-DROWN` = {
                `adjust`<- function(sheet) {
                    tab<- tabs[[sheet]]

                    colnames(tab)<- c("Year", "Drowning Rate (per 100k)")

                    tab %>% filter(!is.na(`Year`))
                }
            },
            `B.20-REFUND` = {
                `adjust`<- function(sheet) {
                    tab<- tabs[[sheet]]

                    colnames(tab)<- c(
                        "Year",
                        "IRS Refunds (USD, millions)",
                        "Population (thousands)"
                    )

                    tab %>% filter(!is.na(`Year`))
                }
            }
        )

        `adjust`(sheet)
    }

    sheets<- names(tabs)
    tabs<- lapply(sheets, make_adjustments)
    names(tabs)<- sheets

    tabs
}
`read_us_data_on_consumption_and_income`<- function() {
    # source:
    # "Learning and Practicing Econometrics"
    #
    #    William E. Griffiths, R. Carter Hill and George G. Judge
    #    ISBN: 0471513644
    #    896 pages
    #    Wiley, 1993
    #
    # http://www.wiley.com/WileyCDA/WileyTitle/productCd-0471513644.html

    data<- list(
        C=c(4596,4655,4637,4609,
            4627,4658,4646,4660,
            4644,4681,4651,4665,
            4718,4794,4966,4849,
            4921,4810,4834,4841,
            4838,4905,4912,5000,
            5043,5048,5019,4998,
            5001,5027,5082,5149,
            5206,5270,5299,5362,
            5354,5339,5330,5359,
            5372,5361,5374,5366,
            5294,5328,5385,5416,
            5479,5530,5557,5550,
            5549,5601,5553,5540,
            5528,5584,5565,5641,
            5677,5712,5740,5785,
            5810,5824,5883,5901,
            5996,6077,6161,6159,
            6244,6298,6384,6519,
            6576,6587,6630,6631,
            6664,6733,6750,6771,
            6891,6966,7072,7082,
            7143,7172,7192,7230,
            7259,7270,7308,7264,
            7354,7392,7409,7479,
            7565,7671,7755,7916,
            8002,7970,7986,7930,
            7825,7852,7878,7754,
            7797,7909,7970,8028,
            8179,8220,8291,8395,
            8495,8495,8558,8654,
            8673,8821,8839,8899,
            8909,8874,8907,8925,
            8890,8681,8752,8809),
            Y=c(4886,4766,4855,4774,
            4869,4993,5070,5059,
            4938,4916,4903,4897,
            5228,5183,5184,5275,
            5215,5333,5347,5332,
            5305,5335,5427,5441,
            5486,5546,5510,5507,
            5489,5440,5502,5581,
            5590,5679,5751,5827,
            5851,5871,5874,5917,
            5902,5918,5923,5888,
            5827,5848,5948,6000,
            5991,6066,6010,6034,
            6051,6064,6036,5994,
            6028,6097,6124,6203,
            6243,6272,6285,6286,
            6324,6343,6385,6458,
            6566,6722,6783,6834,
            6859,6942,7106,7199,
            7224,7247,7301,7348,
            7446,7498,7538,7570,
            7653,7756,7737,7766,
            7756,7829,7969,8009,
            8026,8139,8208,8160,
            8261,8352,8337,8338,
            8372,8432,8571,8871,
            8965,9013,9059,9130,
            8948,8840,8866,8814,
            8707,9115,8947,9007,
            9125,9151,9187,9237,
            9237,9318,9461,9506,
            9599,9730,9763,9849,
            9889,9819,9820,9790,
            9816,9611,9676,9786),
        N=136
    )

	tab<- as.data.frame(data[c("C", "Y")])
	tab<- cbind(
        time=seq(as.yearqtr("1947 Q1"), len=nrow(tab), by=1/4),
        tab
    )

	reg<- lm(C ~ Y, data=tab)
	beta_0<- coef(reg)[1]
	beta_1<- coef(reg)[2]

	tab %<>% mutate(
		e = C - (beta_0 + beta_1*Y)
	)

    tab
}
`get_name`<- function(sheet) {
    ##  FIXME: replace the evil with closure
    info %>% filter(`Sheet`==sheet) %>% select(Name) %>% .[[1]]
}
`get_t_name`<- function(sheet) {
    info %>% filter(`Sheet`==sheet) %>% select(`abscissa`) %>% .[[1]]
}
`get_t`<- function(sheet, start=1, end=nrow(tabs[[sheet]])) {
    tab<- tabs[[sheet]]

    if (missing(start) & missing(end)) {
        out<- tab %>% select(one_of(get_t_name(sheet)))
    }
    else {
        assert(start >= 1, end >= 1, start <= end)

        t<-      get_t(sheet) %>% .[[1]]
        ##  in case start > T
        t_seq<-  seq(t[1], by=diff(t[1:2]), len=end)
        t_seq<-  t_seq[start:end]

        out<- data_frame(
            x = t_seq
        )
        colnames(out)<- get_t_name(sheet)
    }

    out
}
`get_next_t`<- function(sheet, T_new) {
	t<-     get_t(sheet) %>% .[[1]]
    t_new<- seq(last(t), by=diff(t[1:2]), len=T_new + 1)[-1]

	out<- data_frame(
		x = t_new
	)
    colnames(out)<- get_t_name(sheet)

    out
}
`get_y`<- function(sheet, response=NULL) {
    tab<- tabs[[sheet]]

    if (is.null(response)) {
        cn<- colnames(tab)
        response.names<- setdiff(cn, get_t_name(sheet))
        response<- response.names[1]
    }
    else
        assert(response %in% colnames(tab))

    tab[, response]
}
`get_T`<- function(sheet, response=NULL) {
    length(get_y(sheet, response))
}
`get_y_tilde`<- function(sheet, fit) {
	y_tilde<- extract(fit)[["y_tilde"]]

    y_tilde_mean<-      apply(y_tilde, 2, mean)
    T_new<- length(y_tilde_mean)
    y_tilde_quantiles<- apply(y_tilde, 2, quantile, c(.0275,.975))

    get_next_t(sheet, T_new) %>%
        mutate(
			y_tilde_mean = y_tilde_mean,
			y_tilde_lower = y_tilde_quantiles["2.75%", ],
			y_tilde_upper = y_tilde_quantiles["97.5%", ]
	)
}
`plot_ts`<- function(sheet, fmla, type="o", lwd=1.5) {
    par(
        las=1,
        cex=1.0,
        cex.main=1.00,
        cex.axis=0.75,
        cex.lab=1.0,
        mar=c(3, 3, 0, 2),
        mgp=c(4, 1.75, 0),
        mai=rep(1, 4),
        oma=c(0, 0, 0, 0)
    )

    col="steelblue3"
    plot(
        fmla,
        data=tabs[[sheet]],
        main=gsub("Table ", "", get_name(sheet)),
        col="steelblue4",
        lwd=lwd,
        type=type
    )
}
`get_data_block`<- function(
    sheet,
    model.name,
    response=NULL,
    T_new=NULL,
    Lambda=NULL
) {
    assert(
        sheet %in% names(tabs),
        is.character(get_model_filename(model.name))
    )
    ##  input to Stan data block
    .data<- within(list(), {
        y<-           get_y(sheet, response)
        T<-           get_T(sheet, response)
        T_new<-       T_new
        Lambda<-      Lambda
    })

    .data
}
`get_stanfit`<- function(
    .data,
    model.name,
    .iter=2000,
    .chains=3
) {
    assert(
        is.list(.data),
        !is.null(names(.data)),
        all(c("y", "T") %in% names(.data))
    )
    model.file<- get_model_filename(model.name)

    if (length(ix<- grepl("model", names(.data)))) {
        .data[ix]<- NULL
    }
    ##  real fit:
    fit<- stan(
            file=model.file,
            data=.data,
            iter=.iter,
            chains=.chains,
            seed=get_seed()
    )

    fit
}
`show_model_file`<- function(model) {
    model.filename<- get_model_filename(model)
    assert(file.exists(model.filename))

    cat(paste(readLines(model.filename)), sep = '\n')

    invisible()
}
`get_model_filename`<- function(
    model=
        c(
            "linear-trend",
            "mean-only_normal_vectorized",
            "mean-only_normal",
            "mean-only_normal_predict_new_model_block",
            "mean-only_normal_predict_new_generated_quantities",
            "mean-only_normal_predict_new_generated_quantities_mc",
            "mean-varying_normal",
            "mean-varying_normal_predict_new",
            "mean-varying_linear-trend_normal",
            "mean-varying_seasonal_normal",
            "mean-varying_seasonal_normal_1",
            "mean-varying_seasonal_normal_2",
            "mean-varying_seasonal_normal_predict_new",
            "stur"
        )
) {
    model<- match.arg(model)
    model.filename<- file.path(
        "stan",
        sprintf("%s.stan", model)
    )

    model.filename
}
`make_model`<- function(
    sheet,
    model,
    response=NULL,
    T_new=NULL,
    seasonal=NULL,
    force=FALSE,
    iter=2000,
    n.chains=3
) {
    .binfilename<-
        sprintf(
            "stan/%s_%s%s_%d_s%d.rds",
            model,
            sheet,
            ifelse(is.null(response), "", paste0("_", response, sep="")),
            ifelse(is.null(T_new), 0, T_new),
            ifelse(is.null(seasonal), 0, seasonal)
        )

    if (!file.exists(.binfilename) | force==TRUE) {
        ##  construct the environment

        this<- new.env()

        .sheet<-    sheet
        .model<-    model
        .response<- response
        .T_new<-    T_new
        .Lambda<-   seasonal
        .chains<-   n.chains
        .iter<-     iter

        rm(
            sheet,
            model,
            response,
            T_new,
            n.chains,
            iter
        )

        bind_to_env(get_seed, env=this)

        .data<- get_data_block(
            .sheet,
            response=.response,
            model.name=.model,
            T_new=.T_new,
            Lambda=.Lambda
        )
        .model.file<-  get_model_filename(.model)

        .fit<-
            get_stanfit(
                .data,
                model=.model,
                .iter=.iter,
                .chains=.chains
            )

        `binfilename`<- function() {
            .binfilename
        }
        `sheet`<- function() {
            .sheet
        }
        `model`<- function() {
            .model
        }
        `model_filename`<- function() {
            .model.file
        }
        `show_model_file`<- function() {
            cat(paste(readLines(.model.file)), sep = '\n')

            invisible()
        }
        `y`<- function() {
            .data[["y"]]
        }
        `T`<- function() {
            .data[["T"]]
        }
        `T_new`<- function() {
            .data[["T_new"]]
        }
        `T_end`<- function() {
            if (is.null(T_new())) return(T())
            T() + T_new()
        }
        `fit`<- function() {
            .fit
        }
        `extract_par`<- function(par, start, end) {
            assert(any(grepl(par, names(this$fit()))))

            theta<- rstan::extract(this$fit())[[par]]

            if (is.na(dim(theta)[2]))
                theta<- matrix(theta, nc=1)

            theta_mean<-      apply(theta, 2, mean)
            theta_quantiles<- apply(theta, 2, quantile, c(.0275,.975))

            time_index<- get_t(this$sheet(), start=start, end=end)

            out<- time_index %>%
                mutate(
                    theta_mean = theta_mean,
                    theta_lower = theta_quantiles["2.75%", ],
                    theta_upper = theta_quantiles["97.5%", ]
            )
            colnames(out)<- gsub("theta", par, colnames(out))

            out
        }
        `plot_y_tilde`<- function() {
            y_tilde<- extract_par(
                "y_tilde",
                start = this$T() + 1,
                end = this$T_end()
            )
            y_comb<- bind_rows(
                tabs[[this$sheet()]],
                y_tilde
            )

            z<- zoo(
                y_comb[, setdiff(colnames(y_comb), get_t_name(this$sheet()))] %>% as.matrix,
                order.by=y_comb[, get_t_name(this$sheet())] %>% as.matrix
            )
            plot(
                z[, 1:2],
                col=c("steelblue4", "darkgreen"),
                screen=1,
                main=gsub("Table ", "", get_name(this$sheet())),
                xlab=get_t_name(this$sheet()),
                ylab=colnames(tabs[[this$sheet()]])[2],
                ylim=c(min(z, na.rm=TRUE), max(z, na.rm=TRUE)),
                type="o"
            )
            lines(z[, 3], col="limegreen", type="o")
            lines(z[, 4], col="limegreen", type="o")
        }
        bind_to_env(`binfilename`, env=this)
        bind_to_env(`sheet`, env=this)
        bind_to_env(`model`, env=this)
        bind_to_env(`model_filename`, env=this)
        bind_to_env(`show_model_file`, env=this)
        bind_to_env(`y`, env=this)
        bind_to_env(`T`, env=this)
        bind_to_env(`T_new`, env=this)
        bind_to_env(`T_end`, env=this)
        bind_to_env(`fit`, env=this)
        bind_to_env(`extract_par`, env=this)
        bind_to_env(`plot_y_tilde`, env=this)

        class(this)<- "model"

        ## assert(fully_converged(fit))
        saveRDS(this, file=.binfilename)
    }
    else {
        this<- readRDS(.binfilename)
        assert(this$binfilename()==.binfilename)
    }

    this
}
