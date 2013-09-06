################
# 1 Shell basics
################

echo $SHELL

tcsh
exit

which bash

# create a shell script and try to execute it; the first line tells the operating system what shell to use to run the script
#!/bin/bash  
# ls -al *pdf > myPdfs.txt
# we might need to change the permissions (recall from Unit 1)

#####################
# 3 Command history
#####################
!ls
!-1
!-1:p

####################
# 4 Wildcards
####################

cd stat243-fall-2013/lectures

ls *{pdf,sh}

# in my home directory
ls unit[0-9]*
ls unit[0-9]*pdf
  
ls *[!~#]  # don't show automatically-generated backup files

echo cp filename{,old} 

#####################
# 5 Utilities
#####################

cd stat243-fall-2013/lectures
grep ls *sh
grep ^ls *sh
grep ls.- *sh

# in R
# for(i in 1:10000)  write(mean(rpois(100, 1)), file = 'clt.txt', append = TRUE)
tail -f clt.txt


#####################
# 6 Redirection
#####################

# ls 

grep ls *sh > lsStuff.txt

ls | head -5

cut -d',' -f2 mileage2009.csv | sort | uniq | wc
cut -d',' -f2 mileage2009.csv | sort | uniq | nl 

# you won't be able to replicate this as it uses files on my SCF machine
cd /var/tmp/paciorek/extremes/ghcn/ghcnd_all
cut -b1,2,3,4,5,6,7,8,9,10,11,29,37,45,53,61,69,77,85,93,101,109,117,125,133,141,149,157,165,173,181,189,197,205,213,221,229,237,245,253,261,269  AE000041196.dly | grep "S" | less
cut -b29,37,45,53,61,69,77,85,93,101,109,117,125,133,141,149,157,165,173,181,189,197,205,213,221,229,237,245,253,261,269 USC*.dly | grep "S" | less


ls -lt *sh | head -3
grep for `ls -lt *sh | head -4`

files=$(ls)
echo $files



#####################
# 7 Job control
#####################

R
# i = 0; while(i < 1) print(i)
C-c
C-\

matlab -nodesktop -nodisplay < simulate.m > simulate.out & # let's parse what this will do
R --no-save < code.q > code.Rout &   # "R CMD BATCH" is more standard, but this works too

ssh arwen
ps -aux | grep R

# in R
echo 'for(i in 1:1000){' > tmp.R
echo 'x = matrix(rnorm(5000*5000), nr = 5000, nc = 5000)' >> tmp.R
echo ' y = crossprod(x)}' >> tmp.R

nice -19 R CMD BATCH tmp.R tmp.Rout &

# monitor on top and watch CPU and memory use
# notice the priority is 39 = 20 + 19

########################
# 8 Aliases
########################

alias ls="ls -F"
ls
\ls

# here are some aliases in my .bashrc
alias q="exit"
alias tf="tail -f"
alias l="less"
alias res="cd ~/research"
alias todo="emacs ~/todo &"
alias r="R --no-save"  
alias myjobs="ps -eafl | grep paciorek"
alias scf="ssh -X legolas.berkeley.edu"


#########################
# 9 Shell variables
#########################

name="chris"
echo $name
env
echo $HOSTNAME
echo $HOME

cd lectures
export CDPATH=.:~/research:~/teaching:~/teaching/243
cd lectures

# I put the following in my .bashrc
export PS1="\u@\h:\w> "


#########################
# 10 Functions
#########################



function putscf() {
   scp $1 paciorek@bilbo.berkeley.edu:~/$2 
}

echo 'a' >> file.txt
putscf file.txt teaching/243/garbage.txt

# a few functions from my .bashrc

function mounts(){  # remotely mount filesystems I have access to
    sshfs carver.nersc.gov /accounts/gen/vis/paciorek/nersc
    sshfs bilbo.berkeley.edu: /accounts/gen/vis/paciorek/scf
}

function putweb() {
    scp $1 paciorek@bilbo.berkeley.edu:/mirror/data/pub/users/paciorek/$2
}

function e() {
    emacs $1 &
}

function enw() {
    emacs -nw $1 
}

function l2h(){
    latex2html $1.tex -local_icons -long_titles 5
}


#########################
# 11 If/then/else
#########################

# niceR shortcut for nicing R jobs 
# usage: niceR inputRfile outputRfile 
# Author: Brian Caffo (Johns Hopkins Biostat)
# Date: 10/01/03 

function niceR(){
    # submits nice'd R jobs
# syntax of a function call: niceR file.r Rout
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

#########################
# 12 For loops
#########################

for file in $(ls *txt)
do
   mv $file ${file/.txt/.R} # this syntax replaces .txt with .q in $file
done


# example of bash for loop and wget for downloading a collection of files on the web

IFS=: # internal field separator
mths=jan:feb:mar:apr  
# alternatively I could do mths="jan feb mar apr" and not set IFS
for ((yr=1910; yr<=1920; yr++))
do
    for mth in $mths
    do
        wget ftp://ftp3.ncdc.noaa.gov/pub/data/3200/${yr}/3200${mth}${yr}
    done
done

# if I want to do some post-processing, do the following instead

IFS=: # internal field separator
mths=jan:feb:mar:apr  
for ((yr=1910; yr<=1920; yr++))
do
    for mth in $mths
    do
        wget ftp://ftp3.ncdc.noaa.gov/pub/data/3200/${yr}/3200${mth}${yr}
        grep PRCP 3200${mth}${yr} >> master${mth} # what does this do?
        rm 3200${mth}${yr} # clean up extraneous files
    done
done


# example of bash for loop for starting jobs

n=100 # if I want to be able to vary n from outside the R program
for(( it=1; it<=100; it++));
do
    echo "n=$n; it=$it; source('base.R')" > tmp-$n-$it.R
    R CMD BATCH --no-save tmp-$n-$it.R sim-n$n-it$it.Rout&
done
# note that base.R should NOT set either 'n' or 'it', but should make use of them, including when creating unique names for output files for each iteration
