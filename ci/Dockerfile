FROM rust:1.71-alpine3.18 AS builder
WORKDIR /qulisp
RUN apk add musl-dev && \
    rustup target add x86_64-unknown-linux-musl
COPY . .
RUN cargo build --release && \
    cargo test --no-run 2> /compilation_log && \
    mv "/qulisp/target/x86_64-unknown-linux-musl/debug/deps/$(grep -o "qulisp-[^)]*" /compilation_log)" "/qulisp/test" && \
    mv "/qulisp/target/x86_64-unknown-linux-musl/release/qulisp" "/qulisp/qulisp"

FROM scratch AS test
    COPY --from=builder /qulisp/test ./test
    COPY --from=builder /qulisp/qulisp ./qulisp
    RUN [ "/test" ]

FROM scratch AS extractor
    COPY --from=test ./qulisp ./qulisp