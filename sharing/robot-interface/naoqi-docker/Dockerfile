# Using Ubuntu 17.04 as specified by the NAOqi docs
FROM ubuntu:17.04
MAINTAINER https://twitter.com/MODatUniSA

# Install pip
RUN apt-get update && apt-get install -y python-pip

# Install opencv dependencies
RUN apt-get install -y libsm6 libxrender1 libxext6

# Set the working directory to /naoqi
WORKDIR /naoqi

# Copy the NAOqi for Python SDK
ADD pynaoqi-python2.7-2.1.4.13-linux64.tar.gz /naoqi/

# Copy the boost fix
# Kudos https://community.ald.softbankrobotics.com/en/forum/import-issue-pynaoqi-214-ubuntu-7956
ADD boost/* /naoqi/pynaoqi-python2.7-2.1.4.13-linux64/

# Install Python packages
COPY requirements.txt /naoqi/
RUN pip install -r requirements.txt

# Set the path to the SDK
ENV PYTHONPATH=${PYTHONPATH}:/naoqi/pynaoqi-python2.7-2.1.4.13-linux64/
ENV LD_LIBRARY_PATH="/naoqi/pynaoqi-python2.7-2.1.4.13-linux64:$LD_LIBRARY_PATH"

