# example of bash for loop and wget for downloading a collection of files on the web
# usage: ./forloopDownload.sh
# Author: Chris Paciorek
# Date: July 28, 2011



IFS=: # internal field separator
mths=jan:feb:mar:apr  # I don't know how to specify a vector of characters in bash, so I'll get around this using the : as a delimiter and the bash for loop will automatically do the looping over the elements
for ((yr=1910; yr<=1920; yr++))
do
    for mth in $mths
    do
        wget ftp://ftp3.ncdc.noaa.gov/pub/data/3200/${yr}/3200${mth}${yr}
    done
done

# if I want to do some post-processing, do the following instead

IFS=: # internal field separator
mths=jan:feb:mar:apr  # I don't know how to specify a vector of characters in bash, so I'll get around this using the : as a delimiter and the bash for loop will automatically do the looping over the elements
for ((yr=1910; yr<=1920; yr++))
do
    for mth in $mths
    do
        wget ftp://ftp3.ncdc.noaa.gov/pub/data/3200/${yr}/3200${mth}${yr}
        grep PRCP 3200${mth}${yr} >> master${mth} # what does this do?
        rm 3200${mth}${yr} # clean up extraneous files
    done
done
