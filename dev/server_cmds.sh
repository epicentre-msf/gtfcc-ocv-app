# sync data dir with server
rsync -zavh ~/epicentre/malnut-dashboard/data sp:/home/epicentre/malnut-dashboard

# clear cache
ssh shinyproxy "rm -rf /home/epicentre/malnut-dashboard/.cache/*"

# sync local db with server
rsync ~/epicentre/malnut-dashboard/data/malnut.db sp:/home/epicentre/malnut-dashboard/data/
# sync shapefiles
rsync -zavh --include '*.rds' --include='*/' --exclude='*' ~/epicentre/malnut-dashboard/data/ sp:/home/epicentre/malnut-dashboard/data/

# https://reports.msf.net/testing/
docker run --rm -p 5858:3838 -v /home/epicentre/malnut-dashboard:/root/app epicentremsf/shinytvgeo R -e "shiny::runApp('/root/app', port = 3838, host = '0.0.0.0')"
