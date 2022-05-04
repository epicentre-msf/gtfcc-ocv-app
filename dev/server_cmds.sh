# sync data dir with server
rsync -zavh /Users/paul/epicentre/gtfcc-ocv-app/data shinyproxy:/srv/shiny-server/ocv

# clear cache
ssh shinyproxy "rm -rf /home/epicentre/gtffc-ocv-app/.cache/*"

# sync local db with server
rsync ~/epicentre/gtffc-ocv-app/data/malnut.db sp:/home/epicentre/gtffc-ocv-app/data/

# https://reports.msf.net/testing/
docker run --rm -p 5858:3838 -v /home/epicentre/gtffc-ocv-app:/root/app epicentremsf/shinytvgeo R -e "shiny::runApp('/root/app', port = 3838, host = '0.0.0.0')"
