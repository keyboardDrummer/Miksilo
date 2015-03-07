class FieldAssignment {
    int myField;

    public static void main(String[] args) {
        FieldAssignment me = new FieldAssignment();
        me.assignMyField();
        System.out.print(me.myField);
    }

    void assignMyField() {
        this.myField = 3;
    }
}