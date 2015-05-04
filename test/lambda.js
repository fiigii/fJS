let x = function (x)
			function (y) {
				var a = 2;
				var b = 3;
				if x == y then a + b else a - b;
			};
	y = 1;
in x(y)(2);

{
	var f = function (n)
				if n == 0
				then 1
				else n * x(n - 1);
	var x = function (n)
				if n == 0
				then 1
				else n * f(n - 1);
	f(4);
};