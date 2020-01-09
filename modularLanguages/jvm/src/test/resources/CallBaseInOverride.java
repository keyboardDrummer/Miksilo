class CallBaseInOverride {
    public static void main(String[] args) {
        Object me = new CallBaseInOverride();
        System.out.print(me.equals(me) ? 1 : 0);
    }

    public boolean equals(Object obj) {
        return super.equals(obj);
    }
}