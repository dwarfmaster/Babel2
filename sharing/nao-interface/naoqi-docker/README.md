# Naoqi Docker Installation

Use Docker to build the [NAOqi Python SDK](http://doc.aldebaran.com/2-1/dev/python/install_guide.html) for Ubuntu 17.04 and NAOqi 2.1.4.13

## Installation

* Install [Docker](https://www.docker.com).
* Move into the directory `$ cd naoqi-docker`
* Download the [Python SDK for Linux](http://doc.aldebaran.com/2-1/dev/community_software.html#retrieving-software). The file should be something like `pynaoqi-python2.7-2.1.4.13-linux64.tar.gz`.
* Build the container `$ docker build -t naoqi-python .`