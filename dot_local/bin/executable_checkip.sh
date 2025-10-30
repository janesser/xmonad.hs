## get public ip
# geoiplookup $(checkip.sh)
# go get -u github.com/schachmat/wego
curl -s checkip.dyndns.org|sed -e 's/.*Current IP Address: //' -e 's/<.*$//'
