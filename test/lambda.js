var g = function (x, y, t) {
	var a = 2
	var b = 3
	if (x == y) a + b else a - b
}

var f = function (n) {
	if (n == 0) 1
	else n * f(n - 1)
}
	
var main = function (arg) {
	f(g(1, 1, 1))
}