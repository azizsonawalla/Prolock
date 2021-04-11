:- module(prolockErrors, [notImplemented/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prolock related errors %
%%%%%%%%%%%%%%%%%%%%%%%%%%

notImplemented(Location) :- write("Not yet implemented function in: "), write(Location), nl, fail.