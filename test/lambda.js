function f (n) {
	if (n == 0) 1
	else n * f(n - 1)
};

function length (list) {
	if (nil? list) 0
	else 1 + length(tail list)
}

function fix(f) {
	var x = f(x);
	x
}

var elem = function (e, list) {
	if (nil? list) false
	else if (e == head list) true
	else elem(e, tail list)
}

function map (f, list) {
	if (nil? list) []
	else {
		f(head list) : map(f, tail list)
	}
}

function main (argument) {
	var list = 0 : 1 : [2, 3, 4, 5];
	var element = 10;
	function (x, y, z) {
		if (x) function (i) y(i)
		else function (i) z(i)
	}
}