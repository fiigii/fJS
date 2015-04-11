(function (x : {right : Bool -> Top}) x.right(true)) 
	({left => true, right => function (x : Top) x});

(function (x : {right : Bool -> Bool}) x.right(true)) 
	({left => true, right => function (x : Bool) x});

(function (x : Bool) x ? {a => true, b => 1} : {b => 2, c => false})
 	(true).a;

(function (x : Bool) x ? {a => true, b => 1} : {b => 2, c => false})
	(true).b;

function (x : Bool) x ? 
					(function (y : {a: Bool, b: Number}) y): 
					(function (y : {a: Bool, c: Number}) y);

// Ref test
(function (r : {decc: Unit -> Number, incc : Unit -> Number})
	(function (_ : Number) r.decc(unit)) (r.decc(unit)) )
((function (c : Ref Number) {decc => (function (x : Unit) (function (_ : Unit) !c)(c = !c - 1)),
							 incc => (function (x : Unit) (function (_ : Unit) !c)(c = !c + 1))}) (ref 10));

({x => ref {a => 1}, y => ref 9}.x) = {a => 1, b => 2};

(function (source : {a : Ref Number, b: Ref Number}) 
	(function (x : Ref Number) 
			(function (_ : Unit) !(source.a))(x = 199))
		(source.b))
	({a => (ref 0), b => ref 9, c => 77 + 6});