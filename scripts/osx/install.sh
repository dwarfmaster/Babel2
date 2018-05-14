#!/bin/sh

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
	(curl -s "https://raw.githubusercontent.com/EvolutionaryLinguisticsAssociation/Babel2/master/scripts/osx/$NEWFILE" |
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


function brew_install {
	# $1: Command to be checked
	# $2: Name of package
	# $3: Options if any
	command -v $1
	if [ $? -eq 0 ]; then
		echo "$2 detected, skipping."
	else
		echo "$2 has not been found."
		echo "Installing..."
		brew install $2 $3
		if [ $? -eq 0 ]; then
			echo "Installed."
		else
			echo "Something wrong has happened, aborting. Contact the Babel team for help."
			exit 1
		fi
	fi
}

function brew_cask_install {
	# $1: Command to be checked
	# $2: Name of package
	# $3: Options if any
	command -v $1
	if [ $? -eq 0 ]; then
		echo "$2 detected, skipping."
	else
		echo "$2 has not been found."
		echo "Installing..."
		brew cask install $2 $3
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


echo "== 1: OSX Development Tools =="
xcode-select -p
if [ $? -eq 0 ]; then
	echo "Development Tools detected, skipping."
else
	echo "Development Tools have not been found."
	echo "Invoking installer..."
	echo "A dialog should have been displayed."
	echo "Follow the instructions on the dialog to completion before performing any more steps."
	xcode-select --install
	read -n1 -rsp "If everything went OK, press any key to continue (or Ctrl+C to abort): "
	echo
fi
echo


echo "== 2: Homebrew install tool =="
command -v brew
if [ $? -eq 0 ]; then
	echo "Homebrew detected, skipping."
else
	echo "Homebrew has not been found."
	echo "Installing..."
	(ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" &&
	 brew doctor)
	if [ $? -eq 0 ]; then
		echo "Installed."
	else
		echo "Something wrong has happened, aborting. Contact the Babel team for help."
		exit 1
	fi
fi
echo

echo "== 3: CCL Lisp compiler =="
## brew_install "ccl" "clozure-cl"
curl -OL "https://github.com/Clozure/ccl/releases/download/v1.11.5/ccl-1.11.5-darwinx86.tar.gz"
tar -xvzf ccl-1.11.5-darwinx86.tar.gz
mv ./ccl/scripts/ccl /usr/local/bin/
mv ./ccl/scripts/ccl64 /usr/local/bin/
mv ./ccl/ /usr/local/src/
echo


echo "== 4: Gnuplot =="
if [ -d "$Aquaterm.app" ]; then
	echo "AquaTerm detected, skipping"
else
	brew cask install aquaterm
fi
brew_install "gnuplot" "gnuplot" "--with-aquaterm --with-cairo"
echo


echo "== 5: Graphviz =="
brew_install "dot" "graphviz"
echo

echo "== 6: Editors =="
echo "There are several editors you can use for Babel development."
echo "We recommend the installation of Emacs. This is the default."
echo
echo "You can also use the professional editor Lispworks for development."
echo "However, the free version of Lispworks won't work with Babel due to memory limits."
echo "We can't install Lispworks for you, but we can set up the config for Babel."
echo
echo "Note that, when choosing Sublime Text, this will not be installed through this script."
echo "You can go to Sublime Text's website and download the latest version there."
echo "Also note that you will need to install additional Sublime Text packages to enjoy the full Lisp experience"
echo "First, install Package Control. Use this to install the following packages:"
echo "SublimeREPL, rainbowth, Load File to REPL, BracketHighlighter and lispindent"
echo "PLEASE BE NOTED that development in Sublime Text is not fully supported by the Babel2 team"
echo
read -n1 -rsp "Press S to skip, L for Lispworks, Ctrl+C to abort, or any other key for Emacs: " KEY
echo
if [ "$KEY" = 'S' -o "$KEY" = 's' ]; then
	echo "Skipping editor."
	echo
elif [ "$KEY" = 'L' -o "$KEY" = 'l' ]; then
	echo
	echo "== 6: Lispworks settings =="
	home_settings ".lispworks" "lispworks-babel" "Lispworks"
	echo
else
	echo
	echo "== 6: Emacs editor =="
	brew tap railwaycat/emacsmacport
	if [ -d "$Emacs.app" ]; then
		echo "Emacs detected, skipping"
	else
		brew install emacs-mac
		ln -s /usr/local/opt/emacs-mac/Emacs.app /Applications
	fi
	echo


	echo "== 6a: Slime plugin =="
	# see if there is a path to load slime from somewhere
	(cat ~/.emacs | sed -Ene "s/\(add-to-list \'load-path \"(.*)\"\).*$/\1/p" | grep slime ||
	# check also default loading folder ~/.emacs.d/
	find ~/.emacs.d -type f -name '*slime*')
	if [ $? -eq 0 ]; then
		echo "Slime detected, skipping."
	else
		echo "Slime has not been found."
		echo "Installing..."
		(cd ~ && git clone https://github.com/slime/slime.git)
		if [ $? -eq 0 ]; then
			echo "Installed."
		else
			echo "Something wrong has happened, aborting. Contact the Babel team for help."
			exit 1
		fi
	fi
	echo


	echo "== 6b: Emacs/Slime settings =="
	home_settings ".emacs" "emacs-babel" "Emacs"
	echo
fi

echo "== 7: Babel2 framework =="
ls ~/Babel2
if [ $? -eq 0 ]; then
	echo "Babel2 detected, skipping."
else
	echo "Babel2 has not been found."
	echo "You can download Babel2 as an archive or clone it from Git"
	echo "To get the latest version, we recommend cloning from Git"
	read -n1 -rsp "Press Z for the archive or C for the Git clone: " KEY
	if [ "$KEY" = 'Z' -o "$KEY" = 'z' ]; then
		echo "Downloading the archive"
		(curl -OL https://github.com/EvolutionaryLinguisticsAssociation/Babel2/archive/v2.0.6.tar.gz &&
			mkdir ~/Babel2 &&
			tar -xvzf v2.0.6.tar.gz -C ~/Babel2 --strip-components 1
			rm v2.0.6.tar.gz)
		echo "Installed."
	elif [ "$KEY" = 'C' -o "$KEY" = 'c' ]; then
		echo "Cloning the git repository"
		brew_install "git" "git"
		(git clone https://github.com/EvolutionaryLinguisticsAssociation/Babel2.git ~/Babel2)
		echo "Installed"
	fi
fi
echo


echo "== 8: Babel2 config for CCL =="
home_settings ".ccl-init.lisp" "ccl-init-babel.lisp" "CCL init"
echo

echo "== 9: Quicklisp =="
ls ~/quicklisp
if [ $? -eq 0 ]; then
	echo "Quicklisp detected, skipping."
else
	echo "Quicklisp has not been found"
	echo "Installing..."
	curl -O "https://beta.quicklisp.org/quicklisp.lisp"
	echo "If CCL does not exit automaticlly, enter (ccl:quit)"
	ccl -l quicklisp.lisp -e "(quicklisp-quickstart:install)(ql:quickload :cl-ppcre)(ql:quickload :cl-who)(quit)" -b
	rm quicklisp.lisp
	echo "Installed."
fi

echo "== 10: Testing babel2 =="
# We omit the browser test because it entails keeping ccl running and it complicates matters
# it usually never fails anyway (compared to possible graphviz/gnuplot issues)
echo "The test consists in opening a PDF file with a diagram and displaying a window with a sinus function."
read -n1 -rsp "Press S to skip, Ctrl+C to abort, or any other key to run the test: " KEY
echo
if [ "$KEY" = 'S' -o "$KEY" = 's' ]; then
	echo "Skipping test."
else
	echo "Give the system some time to run the tests. It shouldn't take more than ten seconds..."
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

echo "-- 11: Warning! =="
echo "On some versions of mac OS, Apple has decided to disable local web servers by default."
echo "In order to use the Babel2 web interface, this needs to be enabled again."
echo "This can be done by following these instructions:"
echo "---------------------------------------------"
echo "https://discussions.apple.com/docs/DOC-12034"
echo "---------------------------------------------"
echo "After following these instructions, restart your machine to make sure the changes took place."
echo


echo
echo "Setup has finished. Happy hacking!"
echo
