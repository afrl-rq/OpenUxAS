#! /bin/sh

if [[ $# -ne 1 ]]; then
    echo "$0: The desired service unit name is required, eg Route_Aggregator"
    exit 4
fi

file=template.txt
service_unit_name=$1

sed -i 's/<Service_Name>/'$service_unit_name'/g' $file

servicenameString=$(echo $1 | awk '{ gsub("_","",$0); print $0 }')
                    
sed -i 's/<ServiceName>/'$servicenameString'/' $file

gnatchop $file
