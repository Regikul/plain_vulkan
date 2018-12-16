FROM erlang:20.2.4
LABEL mainteiner="Maksim Krzhemenevskiy <regikul@gmail.com>"

RUN wget https://github.com/erlang/rebar3/releases/download/3.6.2/rebar3 \
    && chmod +x rebar3 \
    && ./rebar3 local install

RUN curl https://sh.rustup.rs -sSf | sh -s -- -y

RUN apt-get update && apt-get install libvulkan-dev cmake -y

RUN git clone https://github.com/KhronosGroup/glslang.git \
    && cd glslang \
    && git clone https://github.com/google/googletest.git External/googletest \
    && ./update_glslang_sources.py \
    && mkdir -p _build \
    && cd _build \
    && cmake -DCMAKE_BUILD_TYPE=Release ..\
    && make -j install
