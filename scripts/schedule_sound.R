# script to do something at a scheduled time:

# http://www.bnosac.be/index.php/blog/64-scheduling-r-scripts-and-processes-on-windows-and-unix-linux

library(cronR)
f <- system.file(package = "cronR", "extdata", "helloworld.R")

cmd <- cron_rscript(rscript = "scripts/play_sounds.R", cmd = "/Users/ryanpeek/Documents/github/test_projects/scripts/", rscript_log = "./jobs/")

## Every minute
cron_add(cmd, frequency = 'minutely', id = 'job1', description = 'mario')

## Every hour at 20 past the hour on Monday and Tuesday
cron_add(cmd, frequency = 'hourly', id = 'job2', at = '00:20', description = 'Weather', days_of_week = c(1, 2))

## Every day at 14h20 on Sunday, Wednesday and Friday
cron_add(cmd, frequency = 'daily', id = 'job3', at = '14:20', days_of_week = c(0, 3, 5))

## Every starting day of the month at 10h30
cron_add(cmd, frequency = 'monthly', id = 'job4', at = '10:30', days_of_month = 'first', days_of_week = '*')

## Get all the jobs
cron_ls()
## Remove all scheduled jobs
cron_clear(ask=FALSE)
