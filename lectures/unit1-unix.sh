#######################################
# 3: Files and directories
#######################################

# login and copying to a remote machine

ssh -X bilbo
ssh -X paciorek@bilbo.berkeley.edu

pwd

ls

cd /accounts/gen/vis/paciorek/teaching/243
cd ~paciorek/teaching/243
cd ~/teaching/243
cd 243 # provided I am in 'teaching'
cd ..
cd -

cp
cp -r
cp -rp # preserves timestamps and other metainfo (VERY handy for tracing your workflows if you move files between machines)
mkdir
rm
rm -r  # recursively remove a whole directory structure
rm -rf # CAREFUL! In particular, make sure you know what directory you are in

# file transfer between machines
scp file.txt paciorek@bilbo.berkeley.edu:~/research/.
scp paciorek@bilbo.berkeley.edu:/data/file.txt ~/research/renamed.txt

# changing permissions on a file
chmod ugo+x myProg # myProg should be compiled code or a shell script

chmod ugo+rw code.q

chmod go-w myThesisCode.q


zip files.zip a.txt b.txt c.txt
gzip a.txt b.txt c.txt # will create a.txt.gz, b.txt.gz,  c.txt.gz
tar -cvf files.tar myDirectory
tar -cvzf files.tgz myDirectory  
tar -xvf files.tar
tar -xvzf files.tgz
gzip -cd file.gz | less
zcat file.zip | less

# disk usage
df -h
du -h
quota -s

##########################################
# 4: A variety of UNIX tools/capabilities
##########################################

man cp

which R
which matlab
which maple

emacs -nw file.txt
# let's play around with some of the keystrokes in emacs

/proc/meminfo
/proc/cpuinfo
/etc/issue

grep processor /proc/cpuinfo
