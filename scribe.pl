:- set_prolog_flag(double_quotes,chars).

%%
%% Tokenizer
%%
word(X) --> word_segment(X), { length(X, N), N > 0 }.

punctuation(',', comma).
punctuation('.', period).

word_segment([X|Xs]) -->
  word_char(X),
  { forall(punctuation(P, _), \+ X = P), \+ member(X, "'\"':@#$&*()[]~`+-|\\/?") },
  !,
  word_segment(Xs).
word_segment([]) --> [].

word_char(X) --> [X], { char_type(X, alnum) }.

ws --> spaces(X), { length(X, N), N > 0 }.

spaces([X|Xs]) --> space(X), !, spaces(Xs).
spaces([]) --> [].

space(X) --> [X], {char_type(X, space)}.
char(X) --> [X], {\+ char_type(X, space)}.


tokenize([word(X)|Xs]) --> word(X), tokenize(Xs).
tokenize([new_term(X)|Xs]) --> ['"'], word(X), ['"'], tokenize(Xs).
tokenize([punc(P)|Xs]) --> [X], { punctuation(X, P) }, tokenize(Xs).
tokenize(Xs) --> ws, tokenize(Xs).
tokenize([]) --> [].

clean_token(word(Chars), word(Atom)) :- atom_chars(Atom, Chars), !.
clean_token(new_term(Chars), new_term(Atom)) :- atom_chars(Atom, Chars), !.
clean_token(X, X).


scribe(Text, Result, Errors) :-
  phrase(tokenize(Tokens0), Text, Out), !,
  maplist(clean_token, Tokens0, Tokens),
  write(tokens(Tokens)), nl,
  (
    Out = [] ->
    init_db(Db),
    write(db([Db | Tokens])), nl,
    phrase(spec, [Db | Tokens], Out2),
    nl,write(out2(Out2)),
    (
      Out2 = [Result] ->
      Errors = []
      ;
      Out2 = [_, Err | _],
      Errors = [Err]
    )
    ;
    Out = [X | _],
    Errors = [unexpected_token(X)]
  ).

init_db(db(Rows)) :-
  list_to_ord_set([
    article(a, singular),
    article(an, singular),
    article(the, plural),
    article(the, singular),
    article(these, plural),

    verb(is, singular)
  ], Rows).

%% TODO: https://github.com/rails/rails/blob/f95c0b7e96eb36bc3efc0c5beffbb9e84ea664e4/activesupport/lib/active_support/inflections.rb


%%
%% Parser
%%
match(Term), [db(S)] --> [db(S), Term].
state(S), [db(S)] --> [db(S)].
update(S), [db(S)] --> [db(_)].

spec --> sentence, !, spec.
spec --> [].

sentence --> is_a_rel.

is_a_rel -->
  noun_phrase(Thing1),
  match( word(is) ),
  noun_phrase(Thing2),
  match( punc(period) ),
  state(Db0),
  { ord_add_element(Db0, is_a(Thing1, Thing2), Db) },
  update(Db).

noun_phrase(Thing) -->
  state(Db),
  match( word(Article) ),
  { downcase_atom(Article, A), ord_memberchk(article(A, singular), Db) },
  noun(Thing),
  !.

noun_phrase(Thing) --> noun(Thing).

noun(Thing) -->
  state(Db),
  match( word(Thing) ),
  { ord_memberchk(thing(Thing), Db) -> true; abort(Db, no_such_thing(Thing)) },
  !.
noun(Thing) -->
  state(Db0),
  match( new_term(Thing) ),
  { ord_memberchk(thing(Thing), Db0) -> abort(Db0, already_defined(Thing)); ord_add_element(Db0, thing(Thing), Db) },
  update(Db),
  !.

%% abort(Error) --> state(Db), { abort(Db, Error) }.
abort(Db, Error) :- throw(scribe_parse_fail(Error, db(Db))).

known_term(X) :-
  article(X, _);
  verb(X, _).
