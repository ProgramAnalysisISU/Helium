FROM base/archlinux
RUN pacman -Syu --noconfirm
RUN pacman -S --noconfirm base-devel
RUN pacman -S --noconfirm clang llvm
RUN pacman -S --noconfirm clang-tools-extra
RUN pacman -S --noconfirm python python2
RUN pacman -S --noconfirm docker
RUN pacman -S --noconfirm emacs
RUN pacman -S --noconfirm git cmake

RUN pacman -S --noconfirm ttf-dejavu

# racket
RUN pacman -S --noconfirm racket

# for helium-v1.3
RUN pacman -S --noconfirm gtest doxygen graphviz boost pugixml rapidjson
RUN pacman -S --noconfirm mlocate
RUN pacman -S --noconfirm vim the_silver_searcher

WORKDIR /root/Desktop

RUN git clone https://github.com/lihebi/helium2
RUN cd helium2 && mkdir build && cd build && cmake .. && make -j4
RUN cd helium2 && raco exe -o helium2 helium.rkt

RUN git clone https://github.com/lihebi/helium
RUN cd helium && mkdir build && cd build && cmake .. && make -j4

RUN bash helium/setup.sh && source ~/.bashrc
RUN bash helium2/setup.sh && source ~/.bashrc

RUN git clone https://github.com/lihebi/benchmark
