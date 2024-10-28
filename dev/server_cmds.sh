# sync data dir with server
# rsync -zavh /Users/paul/epicentre/gtfcc-ocv-app/data sp:/srv/shiny-server/ocv
rsync -zavh /Users/paul/epicentre/gtfcc-ocv-app/data episerv:/home/epicentre/gtfcc-ocv-app/

rsync -zavh /Users/paul/epicentre/gtfcc-ocv-app/data episerv:/home/epicentre/gtfcc-ocv-app-dev/
# dev repo
rsync -zavh /Users/paul/epicentre/gtfcc-ocv-app/data episerv:/home/epicentre/gtfcc-ocv-app-dev/
# clear cache
ssh episerv "rm -rf /home/epicentre/gtfcc-ocv-app/.cache/*"
# ssh shinyproxy "rm -rf /srv/shiny-server/ocv/.cache/*"

# pull data on server locally
rsync -zavh episerv:/home/epicentre/gtfcc-ocv-app/data /Users/paul/epicentre/gtfcc-ocv-app/

# https://reports.msf.net/testing/
docker run \
    --rm \
    -p 5858:3838 \
    -v /home/epicentre/gtfcc-ocv-app-dev:/root/app \
    epicentremsf/epishiny:ubuntu \
    R -e "shiny::runApp('/root/app', port = 3838, host = '0.0.0.0')"
