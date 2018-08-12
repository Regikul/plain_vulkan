%%%-------------------------------------------------------------------
%%% @author regikul
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jun 2018 18:02
%%%-------------------------------------------------------------------
-module(plain_vulkan_util).
-author("regikul").

%% API
-export([
  id/1,
  id/2,
  fold_flags/2,
  unfold_flags/2
]).

-type fold_fun(Head, Acc) :: fun((Head, Acc) -> Acc).

-spec id(any()) -> any().
id(X) -> X.

-spec id(any(), any()) -> any().
id(_Value, Acc) -> Acc.

-spec fold_flags([atom()], proplists:proplist()) -> non_neg_integer().
fold_flags(Flags, Desc) ->
  lists:foldl(count_bits_by(Desc), 0, Flags).

-spec count_bits_by(proplists:proplist()) -> fold_fun(atom(), non_neg_integer()).
count_bits_by(FlagDesc) ->
  fun (Flag, Bits) when is_integer(Bits) ->
    case proplists:get_value(Flag, FlagDesc) of
      Value when is_integer(Value) -> Bits bor Value;
      _ -> Bits
    end
  end.

-spec unfold_flags(non_neg_integer(), proplists:proplist()) -> [atom()].
unfold_flags(Bits, Desc) ->
  lists:foldl(generate_flags_on(Bits), [], Desc).

-spec generate_flags_on(pos_integer()) -> fold_fun(proplists:property(), [atom()]).
generate_flags_on(Bits) ->
  fun ({Flag, Bit}, Flags) ->
    case Bit band Bits of
      0 -> Flags;
      Bit -> [Flag | Flags]
    end
  end.
