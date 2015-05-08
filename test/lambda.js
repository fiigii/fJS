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

function main (argument) {
	var f = fix(function(f) function (n) {
		if (n == 0) 1
		else n * f(n - 1)
	});
	var x = 4;
	{
		var l = length([x]) : [x, x, f(x), f(f(x))];
		elem
	}
}