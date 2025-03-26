:- dynamic var_value/2. 

evaluate(N, N):- number(N).

evaluate(Var, Value):- 
    atom(Var), 
    (var_value(Var, Value) -> true ; throw(error(unbound_variable(Var)))).

evaluate(A+B, Result):- 
    evaluate(A, AResult), 
    evaluate(B, BResult), 
    Result is AResult+BResult.

evaluate(A*B, Result):- 
    evaluate(A, AResult), 
    evaluate(B, BResult), 
    Result is AResult*BResult.

evaluate(A-B, Result):- 
    evaluate(A, AResult), 
    evaluate(B, BResult), 
    Result is AResult-BResult.

evaluate(A/B, Result):- 
    evaluate(A, AResult), 
    evaluate(B, BResult),
    (  BResult =:= 0
    -> throw(error(division_by_zero))
    ;  Result is AResult/BResult
    ).

evaluate(Var = Expr, Result):-
    validate_variable(Var),
    evaluate(Expr, Result),
    retractall(var_value(Var, _)),
    asserta(var_value(Var, Result)).

validate_variable(Var):-
    atom(Var),
    atom_chars(Var, [First|Rest]),
    (  member(First, ['_', '$']) 
    ;  char_type(First, lower)
    ),
    forall(member(C, Rest), valid_var_char(C)).

valid_var_char(C):- 
    char_type(C, alnum) 
    ; C = '_'.

handle_eval_error(error(unbound_variable(Var))):- format('Undefined variable ~w~n', [Var]).
handle_eval_error(error(division_by_zero)):- write('Division by zero'), nl.
handle_eval_error(error(invalid_expression(Expr))):- format('Invalid expression ~w~n', [Expr]).
handle_eval_error(Error):- format('Error: ~w~n', [Error]).
handle_read_error(Error):- format('Input error: ~w~n', [Error]).

main:-
    repeat,
    write('Enter an expression or assignment (or "exit." to quit): '),
    flush_output,
    catch(
        read(Expr),
        Error,
        (handle_read_error(Error), fail)
    ),
    (   Expr == exit
    ->  !, nl, write('Exiting calculator'), nl
    ;   catch(
            (   evaluate(Expr, Result)
            ->  (   Expr =.. [=, Var, _]  
                ->  format('~w = ~w~n', [Var, Result])
                ;   format('Result: ~w~n', [Result])
                )
            ;   throw(error(invalid_expression(Expr)))
            ),
            EvalError,
            (handle_eval_error(EvalError), fail)
        )
    ),
    fail.

