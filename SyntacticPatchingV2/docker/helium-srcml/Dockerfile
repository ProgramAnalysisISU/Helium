FROM ubuntu:16.04
# 14.04 is not uesd because it has old version of pugixml

# 18.04 will have libcurl4 installed, but srcml requires
# libcurl3. Installing libcurl3 leads to uninstall of r-base. Thus, we
# have to use old version of ubuntu, say 14.04, which is the last
# version supported by srcml official releases

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -y
RUN apt-get install -y apt-utils
RUN apt-get install -y build-essential  pkg-config cmake autoconf
RUN apt-get install -y git doxygen cloc
RUN apt-get install -y sqlite3 libsqlite3-dev libboost-all-dev libgtest-dev
RUN apt-get install -y libpugixml-dev
RUN apt-get install -y emacs
RUN apt-get install -y wget
RUN apt-get install -y libtinfo-dev libz-dev
RUN apt-get install -y subversion
RUN apt-get install -y locate

RUN apt-get install -y clang
RUN apt-get install -y exuberant-ctags
RUN apt-get install -y r-base
RUN apt-get install -y python3
RUN apt-get install -y z3

# download srcml
RUN wget http://131.123.42.38/lmcrs/beta/srcML-Ubuntu14.04-64.deb -O /tmp/srcml.deb
RUN dpkg -i /tmp/srcml.deb

WORKDIR /tmp/gtest-build
RUN cmake /usr/src/gtest && make && cp libgtest*.a /usr/local/lib


# clone helium
WORKDIR /root
RUN git clone https://github.com/lihebi/helium
WORKDIR helium
RUN git checkout srcml-test
RUN cd lib/ctags* && make && make install
RUN ldconfig
RUN make -j4
RUN bash setup.sh
RUN . ~/.bashrc
RUN make systype.tags
