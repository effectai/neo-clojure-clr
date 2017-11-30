#
# Build step 1: build NEO core
#
FROM microsoft/dotnet:1.1-sdk as neo-builder

ENV NEO_TARGET netstandard1.6
ENV NEO_VERSION 1bbebb1db8fa7e5a72f372e933f65f4815a1911b
ENV NEO_ARTIFACT https://github.com/neo-project/neo/archive/$NEO_VERSION.zip
ENV NOS_ARTIFACT https://github.com/nasser/nostrand/archive/master.zip

RUN apt-get update && apt-get install -y unzip

WORKDIR /usr/local/build
RUN wget $NEO_ARTIFACT && unzip $NEO_VERSION.zip && rm $NEO_VERSION.zip && mv neo-$NEO_VERSION neo
RUN wget $NOS_ARTIFACT && unzip master.zip && rm master.zip && mv nostrand-master nos

WORKDIR /usr/local/build/neo/neo
RUN dotnet restore && dotnet publish -c Release -f $NEO_TARGET \
    && rm -rf bin/Release/$NEO_TARGET/publish/runtimes

#
# Build step 2: application image
#
FROM mono:5.4.1.6
MAINTAINER jeisses <jesse@effect.ai>

ENV NEO_TARGET netstandard1.6

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
       libleveldb-dev \
       sqlite3 \
    && rm -rf /var/lib/apt/lists/*

# Install 1.9 version of libuv
WORKDIR /usr/local/build
RUN curl -O http://ftp.cn.debian.org/debian/pool/main/libu/libuv1/libuv1_1.9.0-1~bpo8+1_amd64.deb \
    && curl -O http://ftp.cn.debian.org/debian/pool/main/libu/libuv1/libuv1-dev_1.9.0-1~bpo8+1_amd64.deb \
    && dpkg -i libuv1_1.9.0-1~bpo8+1_amd64.deb \
    && dpkg -i libuv1-dev_1.9.0-1~bpo8+1_amd64.deb \
    && rm *.deb

# Install nostrand
COPY --from=0 /usr/local/build/nos /opt/nos
WORKDIR /opt/nos
RUN xbuild && ln -s `pwd`/bin/Release/nos /usr/local/bin/

WORKDIR /usr/local/app

COPY --from=0 /usr/local/build/neo/neo/bin/Release/$NEO_TARGET/publish deps/ext/neo

RUN mkdir -p deps/nuget && cd deps/nuget \
    && nuget install BouncyCastle && nuget install clojure.data.json

COPY . .

# we cant change the load path of protocol.json in NEO, so copy over for now
RUN cp protocol.json /opt/nos/bin/Release

CMD ["nos", "tasks/repl"]
