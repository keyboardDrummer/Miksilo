class WhileeWithComment
{
    /* SomeComment1 */
    public /* inner comment */ static void main(String[] args)
    {
        int i = 0;
        /* SomeComment2 */
        while(i < 3)
        {
            /* SomeComment3 */
            i = /* SomeComment4 */ i;
            i = i + 1;
            i = i + /* SomeComment5 */ 1;
            i = /* SomeComment6 */ i + 1;
        }
        System.out.print(i);
    }
}