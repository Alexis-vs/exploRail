#!/bin/sh
cd exploRail/

Rscript R/scrap.R

# ## Push data on exploRail github repository
# git add -A
# git commit -m 'test crontab on .sh'
# git push origin main
