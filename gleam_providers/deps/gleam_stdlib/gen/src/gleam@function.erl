-module(gleam@function).
-compile(no_auto_import).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, rescue/1]).

compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

identity(X) ->
    X.

rescue(A) ->
    gleam_stdlib:rescue(A).
