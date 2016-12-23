# zshenv
#
# Always-loaded ZSH new shell creation script.  Sets up zsh for noninteractive
# use.

path=()
for i in ~/bin /opt/local/bin /usr/dcs/software/supported/bin /usr/dcs/applications/prify/bin /usr/dcs/software/licensed/bin /usr/dcs/software/evaluation/bin /usr/dcs/software/unsupported/bin /usr/dcs/software/old/bin /usr/dcs/csil/bin /local/bin /usr/afsws/bin /usr/psc/bin /usr/psc/gnu/bin /bin /sbin /usr/bin /usr/sbin /usr/local/bin /usr/local/sbin /usr/X11R6/bin /usr/games /usr/local/games /usr/ucb /usr/dt/bin /usr/ccs/bin /usr/bin/X11 /usr/openwin/bin /usr/kerberos/bin /usr/kerberos/sbin /opt/apache-maven-3.0.4/bin; do
  [[ -d $i ]] && path=($path $i)
done

if [ -d /etc/paths.d ]; then
  for f in /etc/paths.d/*(.N); do
    path=($path `cat $f`)
  done
fi

export PATH
