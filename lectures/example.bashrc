# .bashrc

if [ -f ~skel/std.bashrc ]; then
    source ~skel/std.bashrc
fi

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc 
fi

# User specific aliases and functions


alias cf="cd /var/tmp/paciorek/codeforge/spatialstat/krige"

alias tr="cd $HOME/research/trees"
alias jmac="cd $HOME/research/jmac"
alias hei="cd $HOME/research/fusion/hei"
alias sc="cd $HOME/research/spatconf/paper1"
alias nhs="cd $HOME/research/nhs"
alias ext="cd $HOME/research/prabhat/climate/extremes"
alias extd="cd /data/extremes"
alias car="cd $HOME/research/spatconf/carVsTps"

alias 243="cd $HOME/teaching/243"

alias wr="cd write"
alias code="cd code"
alias data="cd data"
alias out="cd output"

alias desk="cd ~/Desktop"

#############################################################################
# this code customizes the prompt to show the machine name and current path
#############################################################################

#export PS1="paciorek@\h:\w> "
export PS1="$(whoami)@\h:\w> "

alias soff="/usr/bin/loffice &"
alias x="xterm&"
alias xx="gnome-terminal&"
alias term="gnome-terminal&"
alias j="e $HOME/journals.txt&"
alias py="python"

alias more="less"
alias m="less"

alias q="exit"
alias tf="tail -f"
alias r="R --no-save"  # /usr/local/bin/R  "/opt/R-2.1.1/bin/R"
alias n="firefox http://www.biostat.harvard.edu/~paciorek &"
alias t="top"

function k(){
	 kill $1
}


function mounts(){
    sshfs carver.nersc.gov: ~/nersc
}

function propagate(){
	 scp $HOME/.bashrc paciorek@hpcc.sph.harvard.edu:~/.
	 putcarv $HOME/.bashrc .bashrc.ext
	 scp $HOME/.bashrc nhcjp@rock.bwh.harvard.edu:~/.
	 scp $HOME/.bashrc paciorek@bilbo.berkeley.edu:~/.
	 cp $HOME/.bashrc ~/scf/infotech/.
	 echo "remember to copy to laptops and to nhair0m on bwh"
}

function p(){
    acroread ~/scf/papers/$1 & 
}

function a(){
    acroread $1&
}

function umounts(){
    fusermount -u ~/nersc
}


function todo(){
    cd $HOME/todo
    cp hours.ods hours.ods.backup
    cp timesheet.txt timesheet.txt.backup
    cp todo todo.backup
    emacs todo&
    emacs timesheet.txt&
    v hours.ods&
    cp $HOME/staff/todo $HOME/staff/todo.backup
    emacs $HOME/staff/todo&
#    emacs ~/shared/todo/todo&
}

function e() {
    emacs $1 &
}

function enw() {
    emacs -nw $1 
}

function dvips() {
    /usr/bin/dvips $1.dvi -t letter -Ppdf -G0 -o 
}

function l(){
    lyx $1.lyx&
}

function la(){
    latex $1.tex
}

function l2h(){
    latex2html $1.tex -local_icons -long_titles 5
}

function gh(){
    gv $1.ps &
}

function xdvi(){
    xdvi $1 &
}

function pdf(){
    xpdf $1.pdf &
}

function p(){
    perl $1 
}

function glook(){
    gzip -cd $1 | less
}

function lsl(){
    ls -l $1 | less
}

function lsal(){
    ls -al $1 | less
}

function putweb() {
    scp $1 paciorek@bilbo.berkeley.edu:/mirror/data/pub/users/paciorek/$2
}

function putcarv() {
    scp $1 paciorek@carver.nersc.gov:~/$2
}
function getcarv() {
    scp paciorek@carver.nersc.gov:~/$1 $2
}

function putscf() {
    scp $1 paciorek@bilbo.berkeley.edu:~/$2
}
function getscf() {
    scp paciorek@bilbo.berkeley.edu:~/$1 $2
}

