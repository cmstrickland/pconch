FROM fukamachi/sbcl:2.3.5
WORKDIR /root/.roswell/local-projects
COPY . .
RUN make clean
RUN make
RUN make install
WORKDIR /pconch
ENTRYPOINT ["/pconch/pconch"]
EXPOSE 2125
EXPOSE 4005
