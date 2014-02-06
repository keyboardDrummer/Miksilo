class Fiboacci {
	public static void main(String[] args)
	{
		System.console().printf("%s", fibonacci(5));
	}

	public static int fibonacci(int index) {
		return index < 2 ? 1 : fibonacci(index-1) + fibonacci(index-2);
	}
}