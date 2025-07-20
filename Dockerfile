FROM alpine:latest AS builder
RUN apk add --no-cache make sbcl curl
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --load quicklisp.lisp \
    --eval "(quicklisp-quickstart:install)"\
    --eval "(ql:add-to-init-file)"\
    --quit

# load some heavy things into a lower layer
RUN sbcl --load ~/quicklisp/setup.lisp \
    --eval "(ql:quickload :alexandria)" \
    --eval "(ql:quickload :hunchentoot)" \
    --eval "(ql:quickload :ironclad)" \
    --eval "(ql:quickload :swank)" \
    --quit
    
COPY . /src
WORKDIR /src
RUN ln -s /src ~/quicklisp/local-projects/pconch
RUN make clean && make

# Runtime stage  
FROM alpine:latest
RUN apk add --no-cache zstd-libs
COPY --from=builder /src/pconch /pconch/pconch
WORKDIR /pconch
ENTRYPOINT ["/pconch/pconch"]
EXPOSE 2125
EXPOSE 4005
