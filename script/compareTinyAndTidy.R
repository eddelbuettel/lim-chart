suppressMessages({
    library(gh)
    library(anytime)
    library(data.table)
    library(ggplot2)
})

#Sys.setenv("TZ" = "UTC")
Sys.setenv("TZ" = "America/Chicago")

getResults <- function(repo, label) {
    res <- gh(paste0("GET /repos/", repo, "/actions/runs?per_page=100"))
    D <- rbindlist(lapply(res$workflow_runs,
                          \(x) data.frame(finish=utctime(x$updated_at, tz="UTC"),
                                          start=utctime(x$run_started_at, tz="UTC"))))
    D[, duration := as.numeric(difftime(finish, start, units="secs"))]
    ## starting July 2023 we get false finish time for earliest starts in data set
    ## leading to contaminated data sets; this filters the bad ones out
    D <- D[duration < 3600*24, ]
    D[, repo := label]
    csvfile <- file.path("csv", paste(label, "csv", sep="."))

    OD <- fread(csvfile)
    ND <- rbind(D[as.IDate(finish) > max(OD[,as.IDate(finish)])], OD)
    fwrite(ND, csvfile)
    ND
}

resD <- getResults("eddelbuettel/lim-tidy", "tidy")
resN <- getResults("eddelbuettel/lim-tiny", "tiny")

## March 1 times are total 'action' run time, not task run-time
resN[repo=="tiny" & trunc(duration) == 675, duration:=77]
resD[repo=="tidy" & trunc(duration) == 513, duration:=196]
## Remove Nov 8 when we one commit borked the usethis yaml resulting in a 10s time (failed)
resD <- resD[as.IDate(finish) == "2022-11-08" & as.ITime(finish) <= "2022-11-08 04:00:00", badrun := TRUE][is.na(badrun)==TRUE,][, badrun := NULL][]

D <- rbind(resD, resN)

p <- ggplot(D, aes(x=finish, y=duration, color=repo)) +
    geom_point() + geom_smooth(method="loess", formula="y ~ x", se=TRUE) +
    ylab("Total Action Run-Time in Seconds") + xlab("") +
    scale_y_continuous(limits = c(0, NA)) +
    tinythemes::theme_ipsum_rc() +
    labs(title="Net time of Continuous Integration: Tiny vs Tidy",
         subtitle="Running a PostgreSQL query at GitHub Action each week, once with RPostgreSQL ('tiny') and once with RPostgres ('tidy')",
         caption=paste0("Runs are scheduled weekly, scripts have been unchanged but for one DB reference update, plus one Actions update. ",
                        "Most recent data point is ", format(D[finish==max(finish), as.Date(finish)]), "."))

if (interactive()) p

filename <- file.path("graph", "tiny_vs_tidy.png")
png(filename, 800, 600)
p
ignoreme <- dev.off()

if (interactive()) cat("Done.\n")
