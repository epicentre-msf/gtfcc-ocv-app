# sync data dir with server
rsync -zavh /Users/paul/epicentre/gtfcc-ocv-app/data sp:/srv/shiny-server/ocv
# clear cache
ssh shinyproxy "rm -rf /srv/shiny-server/ocv/.cache/*"

# https://reports.msf.net/testing/
docker run --rm -p 5858:3838 -v /home/epicentre/gtffc-ocv-app:/root/app epicentremsf/shinytvgeo R -e "shiny::runApp('/root/app', port = 3838, host = '0.0.0.0')"
