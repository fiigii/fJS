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

function generateFrom (start, end) {
	var help = function (start, list) {
		if (start >= end) {
			end : list
		} else {
			start : help(start + 1, list)
		}
	};
	help(start, [])
}

function main (argument) {
	var list = 10 : 1 : [2, 3, 4, 5];
	var element = 10;
	var a = generateFrom(0);
	fix
}