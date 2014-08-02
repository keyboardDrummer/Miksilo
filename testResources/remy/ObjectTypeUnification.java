package remy;

class ObjectTypeUnification {
  public static void main(java.lang.String[] args) {
    Object yo;
    yo = null;
    System.out.print((1 < 2 ? yo : yo).getClass().getSimpleName());
  }
}