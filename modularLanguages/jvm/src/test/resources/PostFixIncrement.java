class PostFixIncrement {
  public static void main(java.lang.String[] args)
  {
    int x = 1;
    x++;
    x = 3 + x++;
    System.out.print(x++);
  }
}