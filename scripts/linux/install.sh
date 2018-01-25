#!/bin/bash

##
## FUNCTIONS DEFINITION
## scroll further down for script flow
##

function home_settings {
	# $1: Name of current file
	# $2: Name of recommended file
	# $3: Name of the setting
	OLDPWD="$PWD"
	cd ~
	OLDFILE="$1"
	NEWFILE="$2"
	NAME="$3"

	# download and make usual replacements
	(wget -O "https://github.com/EvolutionaryLinguisticsAssociation/Babel2/raw/master/scripts/linux/$NEWFILE" |
	# (wget -O - "http://emergent-languages.org/Babel2/downloads/linux/$NEWFILE" |
	 sed -e "s|<USER>|`whoami`|g" |
	 sed -e "s|<CCL>|`which ccl`|g" > "$NEWFILE")
	ls "$OLDFILE"
	if [ $? -eq 0 ]; then
		echo
		echo "You already have a $NAME config file. What do you want to do?"
		KEEPASKING=1
		while [ $KEEPASKING -ne 0 ]; do
			echo
			echo "You can press L to check the settings we suggest with the less tool."
			echo "Otherwise, press W to overwrite the file or A to append to it."
			read -n1 -rsp "You can also press S to skip or Ctrl+C to abort: " KEY
			echo
			if [ "$KEY" = 'L' -o "$KEY" = 'l' ]; then
				less "$NEWFILE"
			else
				KEEPASKING=0
			fi
		done
		if [ "$KEY" = 'S' -o "$KEY" = 's' ]; then
			echo "Skipping step."
		elif [ "$KEY" = 'A' -o "$KEY" = 'a' ]; then
			cat "$NEWFILE" >> "$OLDFILE"
			echo "Appended config."
		elif [ "$KEY" = 'W' -o "$KEY" = 'w' ]; then
			mv "$NEWFILE" "$OLDFILE"
			echo "Overwritten config."
		fi
	else
		mv "$NEWFILE" "$OLDFILE"
		echo "Config placed."
	fi
	rm "$NEWFILE"
	cd "$OLDPWD"
}


function apt_install {
	# $1: Command to be checked
	# $2 & onwards: Name of packages to install
	command -v $1
	if [ $? -eq 0 ]; then
		echo "$2 detected, skipping."
	else
		echo "$2 has not been found."
		echo "Installing..."
		sudo apt-get --yes --force-yes install ${@:2}
		if [ $? -eq 0 ]; then
			echo "Installed."
		else
			echo "Something wrong has happened, aborting. Contact the Babel team for help."
			exit 1
		fi
	fi
}

##
## END OF FUNCTIONS DEFINITION
##


##
## MAIN SCRIPT FLOW
##

echo
echo "Welcome to the Babel 2 installation assistant."
echo "This installer will set up your system for development with Babel 2 (including FCG and IRL)."
echo "Note: If anything goes wrong during the execution, it's safe to try to run it again. \
It will skip the steps already performed."
read -n1 -rsp "Press any key to continue or Ctrl+C to abort: "
echo
echo
echo

echo "== 1: CCL Lisp compiler =="
# there is a cclive package that also installs a ccl command
(command -v ccl && ! dpkg --get-selections | grep cclive)
if [ $? -eq 0 ]; then
	echo "ClozureCL detected, skipping."
else
	CCLPREFIX=/usr/local/share/
	echo "ClozureCL has not been found."
	echo "Installing..."
	(wget https://github.com/Clozure/ccl/releases/download/v1.11.5/ccl-1.11.5-linuxx86.tar.gz &&
	 sudo tar xvzf ccl-1.11.5-linuxx86.tar.gz -C "$CCLPREFIX" &&
	 rm ccl-1.11.5-linuxx86.tar.gz &&
	 sudo sed -i -e "s|/usr/local/src/ccl|$CCLPREFIX/ccl|g" "$CCLPREFIX/ccl/scripts/ccl64" &&
	 sudo ln -s "$CCLPREFIX/ccl/scripts/ccl64" /usr/local/bin/ccl)
	if [ $? -eq 0 ]; then
		echo "Installed."
	else
		rm ccl-1.11.5-linuxx86.tar.gz
		rm -rf "$CCLPREFIX/ccl"
		rm /usr/local/bin/ccl
		echo "Something wrong has happened, aborting. Contact the Babel team for help."
		exit 1
	fi
fi
echo


echo "== 2: Gnuplot =="
# gnuplot-x11 is needed to plot to XWindow
apt_install "gnuplot" "gnuplot" "gnuplot-x11"
echo


echo "== 3: Graphviz =="
apt_install "dot" "graphviz"
echo

echo "== 4: Editors =="
echo "There are several editors you can use for Babel development."
echo "We recommend the installation of Emacs. This is the default."
echo
echo "You can also use the professional editor Lispworks for development."
echo "However, the free version of Lispworks won't work with Babel due to memory limits."
echo "We can't install Lispworks for you, but we can set up the config for Babel."
echo
echo "Note that you will need to install additional packages when using Sublime Text to enjoy the full Lisp experience"
echo "First, install Package Control. Use this to install the following packages:"
echo "SublimeREPL, rainbowth, Load File to REPL, BracketHighlighter and lispindent"
echo "PLEASE BE NOTED that development in Sublime Text is not fully supported by our team"
echo
read -n1 -rsp "Press S to skip, L for Lispworks, T for Sublime Text, Ctrl+C to abort, or any other key for Emacs: " KEY
echo
if [ "$KEY" = 'S' -o "$KEY" = 's' ]; then
	echo "Skipping editor."
	echo
elif [ "$KEY" = 'L' -o "$KEY" = 'l' ]; then
	echo
	echo "== 4: Lispworks settings =="
	home_settings ".lispworks" "lispworks-babel" "Lispworks"
	echo
elif [ "$KEY" = 'T' -o "$KEY" = 't' ]; then
	echo 
	echo "== 4: Sublime Text editor =="
	wget -qO - https://download.sublimetext.com/sublimehq-pub.gpg | sudo apt-key add -
	echo "deb https://download.sublimetext.com/ apt/stable/" | sudo tee /etc/apt/sources.list.d/sublime-text.list
	sudo apt update && sudo apt install sublime-text
	echo
else
	echo
	echo "== 4: Emacs editor =="
	apt_install "emacs" "emacs"
	echo


	echo "== 4a: Slime plugin =="
	# see if there is a path to load slime from somewhere
	(cat ~/.emacs | sed -Ene "s/\(add-to-list 'load-path \"(.*)\"\).*$/\1/p" | grep slime ||
	# check also default loading folder ~/.emacs.d/
	find ~/.emacs.d -type f -name '*slime*')
	if [ $? -eq 0 ]; then
		echo "Slime detected, skipping"
	else
		# apt-get slime insists on using SBCL and can't be reasoned with
		(wget http://common-lisp.net/project/slime/snapshots/slime-current.tgz &&
		 mkdir -p ~/slime &&
		 tar xzf slime-current.tgz -C ~/slime --strip-components 1 &&
		 rm slime-current.tgz)
		if [ $? -eq 0 ]; then
			echo "Installed."
		else
			rm -rf ~/slime
			rm slime-current.tgz
			echo "Something wrong has happened, aborting. Contact the Babel team for help."
			exit 1
		fi
	fi
	echo
	echo "== 4b: Emacs/Slime settings =="
	home_settings ".emacs" "emacs-babel" "Emacs"
	echo
fi


echo "== 5: Babel2 framework =="
ls ~/Babel2
if [ $? -eq 0 ]; then
	echo "Babel2 detected, skipping."
else
	echo "Babel2 has not been found."
	echo "You can download Babel2 as a zip archive or clone it from Git"
	read -n1 -rsp "Press Z for the zip archive or C for the Git clone: " KEY
	if [ "$KEY" = 'Z' -o "$KEY" = 'z' ]; then
		echo "Downloading the zip archive"
		apt_install "unzip" "unzip"
		(wget https://github.com/EvolutionaryLinguisticsAssociation/Babel2/archive/v2.0.0.zip &&
			unzip v2.0.0.zip -d ~/Babel2 &&
			rm v2.0.0.zip)
		echo "Installed."
	elif [ "$KEY" = 'C' -o "$KEY" = 'c' ]; then
		echo "Cloning the git repository"
		apt_install "git" "git"
		(git clone https://github.com/EvolutionaryLinguisticsAssociation/Babel2.git ~/Babel2)
		echo "Installed"
	fi
fi
echo

echo "== 6: Babel2 config for CCL =="
home_settings ".ccl-init.lisp" "ccl-init-babel.lisp" "CCL init"
echo

echo "== 7: Quicklisp =="
ls ~/quicklisp
if [ $? -eq 0 ]; then
	echo "Quicklisp detected, skipping."
else
	echo "Quicklisp has not been found"
	echo "Installing..."
	curl -O "https://beta.quicklisp.org/quicklisp.lisp"
	ccl -l quicklisp.lisp -e "(quicklisp-quickstart:install)(ql:quickload :cl-ppcre)(ql:quickload :cl-who)(ccl:quit)"
	rm quicklisp.lisp
	echo "Installed."
fi

echo "== 8: Testing babel2 =="
# we omit the browser test because it entails keeping ccl running and it complicates matters
# it usually never fails anyway (compared to possible graphviz/gnuplot issues)
echo "The test consists in opening a PDF file with a diagram and displaying a window with a sinus function."
read -n1 -rsp "Press S to skip, Ctrl+C to abort, or any other key to run the test: " KEY
echo
if [ "$KEY" = 'S' -o "$KEY" = 's' ]; then
	echo "Skipping test."
else
	echo "Give the system some time to run the tests. It shouldn't take more than ten seconds..."
	echo "(Close the windows as they are displayed to proceed)"
	ccl -l "~/Babel2/test-babel-installation.lisp" --eval '(quit)' -b
	echo
	echo "If the file didn't open or the window didn't display,"
	echo "there is a problem with your installation. Contact the Babel team for help."
	echo
	read -n1 -rsp "Press any key to continue or Ctrl+C to abort: "
	echo
	echo
fi
echo


echo
echo "Setup has finished. Happy hacking!"
echo
