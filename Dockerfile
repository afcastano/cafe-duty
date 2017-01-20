FROM fpco/stack-build:lts-7.15

COPY . /cafe-duty

WORKDIR /cafe-duty

RUN stack --system-ghc build

CMD ["stack","--system-ghc","exec","cafe-duty"]