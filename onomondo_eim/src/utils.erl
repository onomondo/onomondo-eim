% Author: Philipp Maier <pmaier@sysmocom.de> / sysmocom - s.f.m.c. GmbH

-module(utils).
-export([binary_to_hex/1, hex_to_binary/1, join_binary_list/1, integer_to_bytes/1, lpad_binary/3]).

% Converts a single hex digit (e.g. <<"A">>) into its integer representation.
hexstr_digit_to_int(HexDigit) ->
    [HexDigitInt] = HexDigit,
    HexDigitIntCap = HexDigitInt bor 32,
    case HexDigitIntCap of
	48 -> 0;
	49 -> 1;
	50 -> 2;
	51 -> 3;
	52 -> 4;
	53 -> 5;
	54 -> 6;
	55 -> 7;
	56 -> 8;
	57 -> 9;
	97 -> 10;
	98 -> 11;
	99 -> 12;
       100 -> 13;
       101 -> 14;
       102 -> 15
    end.

% Converts a single hex octet (e.g. <<"1A">>) into its integer representation.
hexstr_octet_to_int(HexOctet) ->
    HexDigits = [ [X] || <<X:8>> <= HexOctet],
    HexDigitsInt = [ hexstr_digit_to_int(X) || X <- HexDigits],
    [HexOctetIntH | T ] = HexDigitsInt,
    [HexOctetIntL | _ ] = T,
    HexOctetIntH * 16 + HexOctetIntL.

% Converts a binary into a printable hex-string (also a binary).
binary_to_hex(Binary) ->
    EncList = [io_lib:format("~2.16.0B",[X]) || <<X:8>> <= Binary ],
    list_to_binary(string:join(io_lib:format("~s", [EncList]), "")).

% Converts a printable hex-string (int the form of a binary) to a binary.
hex_to_binary(HexStr) ->
    OctetsStr = [<<X:16>> || <<X:16>> <= HexStr],
    list_to_binary([ hexstr_octet_to_int(X) || X <- OctetsStr]).

% Joins (concatenates) a list of binaries into a single large binary
join_binary_list([]) ->
    % An empty list will result into an empty binary
    <<>>;
join_binary_list([Single]) ->
    % A list that contains only of a single binary will result into that single binry.
    Single;
join_binary_list([H|T]) ->
    % Lists with multiple binaries get joined into a single large binary
    lists:foldl(fun (Binary, Joined) -> <<Joined/binary, Binary/binary>> end, H, T);
join_binary_list(Binary) ->
    % In case someone uses this function on a binary directly, we transparently return that binary.
    Binary.

% Convert an integer into a binary that contains the integer as a series of bytes (network byte order)
integer_to_bytes(Integer) ->
    % Convert the integer to a list with hex digits (nibbles)
    IntegerDigits = integer_to_list(Integer, 16),
    % Make sure that the list contains an even number of digits (nibbles)
    IntegerDigitsPadded = case length(IntegerDigits) rem 2 of
			      1 ->
				  ["0" | IntegerDigits];
			      _ ->
				  IntegerDigits
			  end,
    % Merge the list of hex digits into a single hex string (binary) and convert that binary into a binary
    IntegerAsHexstring = list_to_binary(IntegerDigitsPadded),
    hex_to_binary(IntegerAsHexstring).

% Padd a binary from the left with a specified padding byte
lpad_binary(Binary, Padding, Length) ->
    if
	byte_size(Binary) >= Length ->
	    Binary;
	true ->
	    BinaryPadded = <<Padding/binary, Binary/binary>>,
	    lpad_binary(BinaryPadded, Padding, Length)
    end.
