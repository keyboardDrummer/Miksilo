class FieldAssignment {
    int myField;

    public static void main(String[] args) {
        new FieldAssignment().assignMyField();
    }

    void assignMyField() {
        this.myField = 3;
    }
}