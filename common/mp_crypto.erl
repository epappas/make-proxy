-module(mp_crypto).
-export([init/1,
         init/2,
         encrypt/2,
         decrypt/2]).

-define(DATALENGTH, 16).
-define(IV, <<"90de3456asxdfrtg">>).

-spec init(iodata()) -> _State.
init(AESKey) -> init(AESKey, ?IV).

-spec init(iodata(), binary()) -> _State.
init(AESKey, AESIV) -> crypto:stream_init(aes_ctr, AESKey, AESIV).

-spec encrypt(_State, binary()) -> {_NewState, binary()}.
encrypt(CryptoState, Binary) -> crypto:stream_encrypt(CryptoState, Binary).


-spec decrypt(_State, binary()) -> {_NewState, binary()}.
decrypt(CryptoState, Binary) -> crypto:stream_decrypt(CryptoState, Binary).


