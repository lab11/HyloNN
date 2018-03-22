# Prepare the image with:
#   docker build -t tensorflow/haskell:v0 docker
FROM tensorflow/tensorflow:1.3.0
LABEL maintainer="TensorFlow authors <tensorflow-haskell@googlegroups.com>"

RUN apt-get update

RUN apt-get install -y \
        # Required by snappy-frames dependency.
        libsnappy-dev \
        # Avoids /usr/bin/ld: cannot find -ltinfo
        libncurses5-dev \
        # Makes stack viable in the container
        libgmp-dev \
        # required to get TF sources 
        git \
        # Required for locales configuration.
        locales

# Our MNIST demo program outputs Unicode characters.
RUN dpkg-reconfigure locales && \
    locale-gen en_US.UTF-8 && \
    update-locale LANG=en_US.UTF-8

# Installs protoc and the libraries.
RUN \
    curl -O -L https://github.com/google/protobuf/releases/download/v3.2.0/protoc-3.2.0-linux-x86_64.zip && \
    unzip -d /usr/local protoc-3.2.0-linux-x86_64.zip bin/protoc && \
    chmod 755 /usr/local/bin/protoc

# Python for tensorflow 
RUN \
    apt-get install python3-numpy python3-dev python3-pip python3-wheel -y

# Bazel deps
RUN \
    apt-get install openjdk-8-jdk pkg-config zip g++ zlib1g-dev unzip -y

# Set up Bazel.

# Running bazel inside a `docker build` command causes trouble, cf:
#   https://github.com/bazelbuild/bazel/issues/134
# The easiest solution is to set up a bazelrc file forcing --batch.
RUN echo "startup --batch" >>/etc/bazel.bazelrc

# Similarly, we need to workaround sandboxing issues:
#   https://github.com/bazelbuild/bazel/issues/418
RUN echo "build --spawn_strategy=standalone --genrule_strategy=standalone" \
    >>/etc/bazel.bazelrc
 
# Install the most recent bazel release.
ENV BAZEL_VERSION 0.4.5
WORKDIR /

RUN mkdir /bazel && \
    cd /bazel && \
    curl -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36" -fSsL -O https://github.com/bazelbuild/bazel/releases/download/$BAZEL_VERSION/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh && \
    curl -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36" -fSsL -o /bazel/LICENSE.txt https://raw.githubusercontent.com/bazelbuild/bazel/master/LICENSE && \
    chmod +x bazel-*.sh && \
    ./bazel-$BAZEL_VERSION-installer-linux-x86_64.sh && \
    cd / && \
    rm -f /bazel/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh

WORKDIR /tensorflow
# Clone tensorflow
RUN git clone --branch=r1.3 --depth=1 https://github.com/tensorflow/tensorflow.git .

# build tensorflow

ENV CI_BUILD_PYTHON python

RUN tensorflow/tools/ci_build/builds/configured CPU \
    bazel build -c opt --cxxopt="-D_GLIBCXX_USE_CXX11_ABI=0" \
    --copt=-march=native //tensorflow:libtensorflow.so
    
RUN \
    cp bazel-bin/tensorflow/libtensorflow.so /usr/local/lib/

# TensorBoard
EXPOSE 6006
# IPython
EXPOSE 8888

RUN ldconfig

ENV LANG en_US.UTF-8
