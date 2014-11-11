-module(mp_crypto).
-export([init/1,
         init/2,
         encrypt/2,
         decrypt/2]).

-define(DATALENGTH, 16).
-define(IV, <<244,44,249,237,76,188,198,195,66,97,184,212,28,112,40,244>>).

-spec init(iodata()) -> _State.
init(AESKey) -> init(AESKey, ?IV).

-spec init(iodata(), binary()) -> _State.
init(AESKey, AESIV) -> crypto:stream_init(aes_ctr, AESKey, AESIV).

-spec encrypt(_State, binary()) -> {_NewState, binary()}.
encrypt(CryptoState, Binary) -> crypto:stream_encrypt(CryptoState, Binary).


-spec decrypt(_State, binary()) -> {_NewState, binary()}.
decrypt(CryptoState, Binary) -> crypto:stream_decrypt(CryptoState, Binary).


