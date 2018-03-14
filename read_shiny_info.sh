#!/bin/bash

# Create an index of a shiny-app server folder and output it as JSON.

# This bash script outputs a character string representing a list of 
# JSON objects with an element per folder in the given input location.
# Each JSON element includes the keys 'name', 'description' and 'readme',
# where the value of 'name' is the name of the folder and the value of
# 'readme' and 'description' are the contents of the corresponding files in each folder.

# The intended use is for creating an index for shinyApps deployed to the shiny-server.
# Author: Tuomo Nieminen 03/2018

cd $@
apps=("*/")
json=""

for app in ${apps[@]}
do
  # description file content, replace newlines with \\n
  if [ -f $app/DESCRIPTION ];then
    desc=$(cat $app/DESCRIPTION | sed -e ':a' -e 'N' -e '$!ba' -e 's/\n/\\n/g')
  else
    desc=""
  fi
  # readme file content
  if [ -f $app/Readme.md ];then
    rdm=$(cat $app/Readme.md)
  else
    rdm=""
  fi
  # append a comma to the JSON unless this is the first iteration
  if [ ! -z "$json" ];then
    json=$json","
  fi
  # 'name' is the folder name
  name=${app/\/}
  
  # build the info and append it to the json object
  appInfo="{\"name\":\""$name"\", \"description\":\""$desc"\", \"readme\":\""$rdm"\"}"
  json=$json$appInfo
done

# print the json
echo ["$json"]
