let funun l = "ciao" ^ " come stai" ^ " Jonny ?";;

let rec interleave (t:'a list) (l:'b list)= match (t, l) with
(t1:: t2 , l1::l2) -> t1 :: l1 :: interleave t2 l2
| (d, []) ->  d
| ([], d) ->  d
| ([],[]) -> []
;;

assert (interleave [6; 22; 20; 5; 34; 21; 24; -12; 8; 32; 33; -13; -11; 18; -2; 1; -11; 30; 11; -1; 28; -9; 8; 7; -16; -13; -23; -12; 28; 15; -21; 15; 1; 10; 0; 13; 21; -18; 6; -1; 2; 2; 8; 25; -2; -12; 9; 16; 34; 15] [20; -25; 3; 25; 27; 21; 9; -5; 26; 23; 31; 32; 32; 17; 4; 11; 30; 1; 0; -18; -17; 3; -22; 28; -10; 22; -17; -15; 9; -13; 7; 9; -10; 25; 31; 32; 1; 22; 3; 24; 15; 1; 15; -16; 1; -2; 7; -10; 5; 10] = [6; 20; 22; -25; 20; 3; 5; 25; 34; 27; 21; 21; 24; 9; -12; -5; 8; 26; 32; 23; 33; 31; -13; 32; -11; 32; 18; 17; -2; 4; 1; 11; -11; 30; 30; 1; 11; 0; -1; -18; 28; -17; -9; 3; 8; -22; 7; 28; -16; -10; -13; 22; -23; -17; -12; -15; 28; 9; 15; -13; -21; 7; 15; 9; 1; -10; 10; 25; 0; 31; 13; 32; 21; 1; -18; 22; 6; 3; -1; 24; 2; 15; 2; 1; 8; 15; 25; -16; -2; 1; -12; -2; 9; 7; 16; -10; 34; 5; 15; 10]);;
assert(interleave [20; 36; 2; 26; 3; 38; 34; 10; 27; 4; 24; 37; 13; 18; 9; 45; 7; 19; 12; 46; 6; 39; 26; 38; 22; 36; 14; 25; 23; 49; 3; 1; 23; 9; 37; 11; 6; 26; 30; 32; 16; 31; 5; 40; 3; 14; 35; 37; 34; 46] [10; 14; 15; 11; 46; 45; 22; 2; 17; 49; 34; 7; 25; 46; 39; 39; 10; 19; 9; 47; 43; 23; 23; 33; 32; 8; 26; 5; 26; 44; 34; 15; 23; 24; 45; 47; 36; 6; 34; 43; 7; 27; 21; 25; 19; 47; 20; 20; 17; 43] = [20; 10; 36; 14; 2; 15; 26; 11; 3; 46; 38; 45; 34; 22; 10; 2; 27; 17; 4; 49; 24; 34; 37; 7; 13; 25; 18; 46; 9; 39; 45; 39; 7; 10; 19; 19; 12; 9; 46; 47; 6; 43; 39; 23; 26; 23; 38; 33; 22; 32; 36; 8; 14; 26; 25; 5; 23; 26; 49; 44; 3; 34; 1; 15; 23; 23; 9; 24; 37; 45; 11; 47; 6; 36; 26; 6; 30; 34; 32; 43; 16; 7; 31; 27; 5; 21; 40; 25; 3; 19; 14; 47; 35; 20; 37; 20; 34; 17; 46; 43]);;
(*assert(interleave [20; 36; 2; 26; 3; 38; 34; 10; 27; 4; 24; 37; 13; 18; 9; 45; 7; 19; 12; 46; 6; 39; 26; 38; 22; 36; 14; 25; 23; 49; 3; 1; 23; 9; 37; 11; 6; 26; 30; 32; 16; 31; 5; 40; 3; 14; 35; 37; 34; 46] [10; 14; 15; 11; 46; 45; 22; 2; 17; 49; 34; 7; 25; 46; 39; 39; 10; 19; 9; 47; 43; 23; 23; 33; 32; 8; 26; 5; 26; 44; 34; 15; 23; 24; 45; 47; 36; 6; 34; 43; 7; 27; 21; 25; 19; 47; 20; 20; 17; 43; 29] = [20; 10; 36; 14; 2; 15; 26; 11; 3; 46; 38; 45; 34; 22; 10; 2; 27; 17; 4; 49; 24; 34; 37; 7; 13; 25; 18; 46; 9; 39; 45; 39; 7; 10; 19; 19; 12; 9; 46; 47; 6; 43; 39; 23; 26; 23; 38; 33; 22; 32; 36; 8; 14; 26; 25; 5; 23; 26; 49; 44; 3; 34; 1; 15; 23; 23; 9; 24; 37; 45; 11; 47; 6; 36; 26; 6; 30; 34; 32; 43; 16; 7; 31; 27; 5; 21; 40; 25; 3; 19; 14; 47; 35; 20; 37; 20; 34; 17; 46; 29; 43]);;*)
(*assert(interleave [20; 36; 2; 26; 3; 38; 34; 10; 27; 4; 24; 37; 13; 18; 9; 45; 7; 19; 12; 46; 6; 39; 26; 38; 22; 36; 14; 25; 23; 49; 3; 1; 23; 9; 37; 11; 6; 26; 30; 32; 16; 31; 5; 40; 3; 14; 35; 37; 34; 46; 29] [10; 14; 15; 11; 46; 45; 22; 2; 17; 49; 34; 7; 25; 46; 39; 39; 10; 19; 9; 47; 43; 23; 23; 33; 32; 8; 26; 5; 26; 44; 34; 15; 23; 24; 45; 47; 36; 6; 34; 43; 7; 27; 21; 25; 19; 47; 20; 20; 17; 29; 43; 29] = [20; 10; 36; 14; 2; 15; 26; 11; 3; 46; 38; 45; 34; 22; 10; 2; 27; 17; 4; 49; 24; 34; 37; 7; 13; 25; 18; 46; 9; 39; 45; 39; 7; 10; 19; 19; 12; 9; 46; 47; 6; 43; 39; 23; 26; 23; 38; 33; 22; 32; 36; 8; 14; 26; 25; 5; 23; 26; 49; 44; 3; 34; 1; 15; 23; 23; 9; 24; 37; 45; 11; 47; 6; 36; 26; 6; 30; 34; 32; 43; 16; 7; 31; 27; 5; 21; 40; 25; 3; 19; 14; 47; 35; 20; 37; 20; 34; 17; 46; 29; 43; 29]);;*)
(*assert(interleave [20; 36; 2; 26; 3; 38; 34; 10; 27; 4; 24; 37; 13; 18; 9; 45; 7; 19; 12; 46; 6; 39; 26; 38; 22; 36; 14; 25; 23; 49; 3; 1; 23; 9; 37; 11; 6; 26; 30; 32; 16; 31; 5; 40; 3; 14; 35; 37; 34; 46; 29; 43] [10; 14; 15; 11; 46; 45; 22; 2; 17; 49; 34; 7; 25; 46; 39; 39; 10; 19; 9; 47; 43; 23; 23; 33; 32; 8; 26; 5; 26; 44; 34; 15; 23; 24; 45; 47; 36; 6; 34; 43; 7; 27; 21; 25; 19; 47; 20; 20; 17; 29; 43; 29] = [20; 10; 36; 14; 2; 15; 26; 11; 3; 46; 38; 45; 34; 22; 10; 2; 27; 17; 4; 49; 24; 34; 37; 7; 13; 25; 18; 46; 9; 39; 45; 39; 7; 10; 19; 19; 12; 9; 46; 47; 6; 43; 39; 23; 26; 23; 38; 33; 22; 32; 36; 8; 14; 26; 25; 5; 23; 26; 49; 44; 3; 34; 1; 15; 23; 23; 9; 24; 37; 45; 11; 47; 6; 36; 26; 6; 30; 34; 32; 43; 16; 7; 31; 27; 5; 21; 40; 25; 3; 19; 14; 47; 35; 20; 37; 20; 34; 17; 46; 29; 29; 43; 29]);;*)
assert(interleave [20; 36; 2; 26; 3; 38; 34; 10; 27; 4; 24; 37; 13; 18; 9; 45; 7; 19; 12; 46; 6; 39; 26; 38; 22; 36; 14; 25; 23; 49; 3; 1; 23; 9; 37; 11; 6; 26; 30; 32; 16; 31; 5; 40; 3; 14; 35; 37; 34; 46; 29; 43] [10; 14; 15; 11; 46; 45; 22; 2; 17; 49; 34; 7; 25; 46; 39; 39; 10; 19; 9; 47; 43; 23; 23; 33; 32; 8; 26; 5; 26; 44; 34; 15; 23; 24; 45; 47; 36; 6; 34; 43; 7; 27; 21; 25; 19; 47; 20; 20; 17; 29; 43] = [20; 10; 36; 14; 2; 15; 26; 11; 3; 46; 38; 45; 34; 22; 10; 2; 27; 17; 4; 49; 24; 34; 37; 7; 13; 25; 18; 46; 9; 39; 45; 39; 7; 10; 19; 19; 12; 9; 46; 47; 6; 43; 39; 23; 26; 23; 38; 33; 22; 32; 36; 8; 14; 26; 25; 5; 23; 26; 49; 44; 3; 34; 1; 15; 23; 23; 9; 24; 37; 45; 11; 47; 6; 36; 26; 6; 30; 34; 32; 43; 16; 7; 31; 27; 5; 21; 40; 25; 3; 19; 14; 47; 35; 20; 37; 20; 34; 17; 46; 29; 29; 43; 43]);;