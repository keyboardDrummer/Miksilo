`include "Bus_pkg.sv"

module use_Bus_trans;

// import Bus_pkg::*;
/*
  Bus_trans t1;
  Bus_trans t2;

  Bus_trans tr;

  initial begin
    t1 = new;
    t1.data = 16'h1234;

    t2 = t1;
    t2.data = 16'hABCD;
    t1.print();

    repeat (3) begin
      tr = new;
      void'( tr.randomize() );
      tr.print();
    end
  end
*/
endmodule