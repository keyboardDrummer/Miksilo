package Bus_pkg;

/*
  typedef logic [15:0] T_addr;
  typedef logic [15:0] T_data;
  typedef enum bit {cmd_Rd, cmd_Wr} T_cmd;
*/
  class Bus_trans;
/*
    static int  next_ID;
    int         ID;
    rand T_cmd  cmd;
    rand T_addr addr;
    rand T_data data;

    function new(T_cmd cmd = cmd_Rd);
      ID = next_ID++;
      addr = 0;
      this.cmd = cmd;
      $write("Created new ");
      print();
    endfunction

    function void print;
      string kind = (cmd==cmd_Rd) ? "Read"  : "Write";
      $display("%s cycle #%0d: A=%h, D=%h",
                kind,     ID,    addr, data);
    endfunction : print
*/
  endclass : Bus_trans

endpackage : Bus_pkg
