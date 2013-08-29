# niceR shortcut for nicing R jobs 
# usage: niceR inputRfile outputRfile 
# Author: Brian Caffo 
# Date: 10/01/03 

function niceR(){
    # submits nice'd R jobs
if [ $# != "2" ]
then 
   echo "usage: niceR inputRfile outputfile" 
elif [ -e "$2" ]
then 
   echo "$2 exists, I won't overwrite" 
elif [ ! -e "$1" ]
then 
   echo "inputRfile $1 does not exist" 
else 
   echo "running R on $1" 
   nice -n 19 R --no-save < $1 &> $2 
fi 
}