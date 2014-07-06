package fibonacciWithoutMain;

class OnlyFibonacci {
	public static int fibonacci(int index) {
		return index < 2 ? 1 : fibonacci(index-1) + fibonacci(index-2);
	}
}