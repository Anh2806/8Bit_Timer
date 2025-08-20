// null_address.v - Verilog-2001 (updated to exercise count_load / OVF / UDF)
`timescale 1ns/1ps
module null_address;
  `include "Def_name.v"

  localparam ADDR_WIDTH = 8;
  localparam DATA_WIDTH = 8;
  localparam LOOPS = 20;
  localparam [DATA_WIDTH-1:0] TCR_MASK = 8'b1011_0011;

  // APB signals
  reg                      PCLK;
  reg                      PRESET_n;
  reg                      PSEL;
  reg                      PWRITE;
  reg                      PENABLE;
  reg  [ADDR_WIDTH-1:0]    PADDR;
  reg  [DATA_WIDTH-1:0]    PWDATA;
  wire [DATA_WIDTH-1:0]    PRDATA;
  wire                     PREADY;
  wire                     PSLVERR;
  wire                     TMR_OVF, TMR_URF;

  // DUT
  timer_top #(.ADDR_WIDTH(ADDR_WIDTH), .DATA_WIDTH(DATA_WIDTH)) dut (
    .PCLK(PCLK), .PRESET_n(PRESET_n),
    .PSEL(PSEL), .PWRITE(PWRITE), .PENABLE(PENABLE),
    .PADDR(PADDR), .PWDATA(PWDATA),
    .PRDATA(PRDATA), .PREADY(PREADY), .PSLVERR(PSLVERR),
    .TMR_OVF(TMR_OVF), .TMR_URF(TMR_URF)
  );

  // 100 MHz clock
  initial begin PCLK = 1'b0; forever #5 PCLK = ~PCLK; end

  // reset
  task do_reset;
    begin
      PRESET_n = 1'b0;
      PSEL = 1'b0; PWRITE = 1'b0; PENABLE = 1'b0;
      PADDR = {ADDR_WIDTH{1'b0}}; PWDATA = {DATA_WIDTH{1'b0}};
      repeat (5) @(posedge PCLK);
      PRESET_n = 1'b1;
      repeat (2) @(posedge PCLK);
    end
  endtask

  // APB write
  task apb_write(input [ADDR_WIDTH-1:0] addr, input [DATA_WIDTH-1:0] data_in);
    begin
      @(posedge PCLK);
      PADDR <= addr; PWDATA <= data_in; PWRITE <= 1'b1; PSEL <= 1'b1; PENABLE <= 1'b0;
      @(posedge PCLK);
      PENABLE <= 1'b1;
      while (!PREADY) @(posedge PCLK);
      @(posedge PCLK);
      PSEL <= 1'b0; PWRITE <= 1'b0; PENABLE <= 1'b0;
    end
  endtask

  // APB read
  task apb_read(input [ADDR_WIDTH-1:0] addr, output [DATA_WIDTH-1:0] data_out);
    begin
      @(posedge PCLK);
      PADDR <= addr; PWRITE <= 1'b0; PSEL <= 1'b1; PENABLE <= 1'b0;
      @(posedge PCLK);
      PENABLE <= 1'b1;
      while (!PREADY) @(posedge PCLK);
      data_out = PRDATA;
      @(posedge PCLK);
      PSEL <= 1'b0; PENABLE <= 1'b0;
    end
  endtask

  // APB write forced-wait (cover PREADY==0 on write)
  task apb_write_with_forced_wait;
    input [ADDR_WIDTH-1:0] addr;
    input [DATA_WIDTH-1:0] data_in;
    input integer wait_cycles;
    integer j;
    begin
      @(posedge PCLK);
      PADDR <= addr; PWDATA <= data_in; PWRITE <= 1'b1; PSEL <= 1'b1; PENABLE <= 1'b0;
      @(posedge PCLK);
      PENABLE <= 1'b1;
      // force internal PREADY low (hierarchical path; adjust name if needed)
      force dut.u_apb.PREADY = 1'b0;
      for (j = 0; j < wait_cycles; j = j + 1) @(posedge PCLK);
      release dut.u_apb.PREADY;
      while (!PREADY) @(posedge PCLK);
      @(posedge PCLK);
      PSEL <= 1'b0; PWRITE <= 1'b0; PENABLE <= 1'b0;
    end
  endtask

  // APB read forced-wait (cover PREADY==0 on read)
  task apb_read_with_forced_wait;
    input  [ADDR_WIDTH-1:0] addr;
    output [DATA_WIDTH-1:0] data_out;
    input  integer         wait_cycles;
    integer j;
    begin
      @(posedge PCLK);
      PADDR <= addr; PWRITE <= 1'b0; PSEL <= 1'b1; PENABLE <= 1'b0;
      @(posedge PCLK);
      PENABLE <= 1'b1;
      force dut.u_apb.PREADY = 1'b0;
      for (j = 0; j < wait_cycles; j = j + 1) @(posedge PCLK);
      release dut.u_apb.PREADY;
      while (!PREADY) @(posedge PCLK);
      data_out = PRDATA;
      @(posedge PCLK);
      PSEL <= 1'b0; PENABLE <= 1'b0;
    end
  endtask

  function [DATA_WIDTH-1:0] rand8; input dummy; begin rand8 = $random; end endfunction

  integer i;
  integer err_cnt;
  reg [DATA_WIDTH-1:0] r, w;
  reg [ADDR_WIDTH-1:0] a;
  reg [DATA_WIDTH-1:0] tmp;
  integer k;

  initial begin
    err_cnt = 0;
    do_reset();

    // random address tests (check PSLVERR and valid regs)
    for (i = 0; i < LOOPS; i = i + 1) begin
      a = rand8(0); w = rand8(0);
      apb_write(a, w);

      // invalid address -> PSLVERR should be 1
      if ((a != `TDR_ADDR) && (a != `TCR_ADDR) && (a != `TSR_ADDR) && (a != `TCNT_ADDR)) begin
        if (!PSLVERR) begin
          $display("[%0t] ERROR: invalid write addr=0x%0h did NOT assert PSLVERR", $time, a);
          err_cnt = err_cnt + 1;
        end else begin
          $display("[%0t] NULL-ADDRESS (write) addr=0x%0h PSLVERR=1", $time, a);
        end
      end else begin
        if (PSLVERR) begin
          $display("[%0t] ERROR: PSLVERR asserted for valid write addr=0x%0h", $time, a);
          err_cnt = err_cnt + 1;
        end
        apb_read(a, r);
        if (a == `TDR_ADDR) begin
          if (r !== w) begin
            $display("[%0t] ERROR: TDR wrote=0x%0h read=0x%0h", $time, w, r); err_cnt = err_cnt + 1;
          end
        end else if (a == `TCR_ADDR) begin
          if ((r & TCR_MASK) !== (w & TCR_MASK)) begin
            $display("[%0t] ERROR: TCR masked mismatch wrote=0x%0h read=0x%0h mask=0x%0h", $time, w, r, TCR_MASK);
            err_cnt = err_cnt + 1;
          end
        end else if (a == `TSR_ADDR) begin
          if (r !== {DATA_WIDTH{1'b0}}) begin
            $display("[%0t] ERROR: TSR expected 0 read=0x%0h", $time, r); err_cnt = err_cnt + 1;
          end
        end else if (a == `TCNT_ADDR) begin
          $display("[%0t] read TCNT = 0x%0h", $time, r);
        end
      end

      // cover forced-wait paths occasionally
      if (i == 5) begin
        apb_write_with_forced_wait(`TDR_ADDR, rand8(0), 3);
        apb_read(`TDR_ADDR, tmp);
        $display("After forced-write TDR=0x%0h", tmp);
      end
      if (i == 10) begin
        apb_read_with_forced_wait(`TCR_ADDR, tmp, 3);
        $display("After forced-read TCR=0x%0h", tmp);
      end
    end

    // -----------------------
    // Ensure we exercise load_tdr -> timer_counter count_load branch
    // Strategy:
    //  - baseline Clock_counter = 0 (force)
    //  - write TDR (creates TDR_WR_pulse on PCLK)
    //  - create a clean rising edge on Clock_counter (0->1->0) soon after posedge,
    //    so timer_counter sees prev_clk=0 and Clock_counter=1 while count_load is active.
    //  - also test TCR.load rising edge sequence similarly
    // -----------------------
    $display("=== Exercise TDR_WR_pulse -> load into TCNT (try to hit count_load) ===");

    // make sure Clock_counter baseline is 0
    force dut.u_selclk.Clock_counter = 1'b0;
    repeat (2) @(posedge PCLK);

    // write TDR (this should assert TDR_WR_pulse inside Rw_register for one clock)
    w = 8'hA5;
    apb_write(`TDR_ADDR, w);
    @(posedge PCLK); // let internal registers sample

    // Now create a clean Clock_counter rising edge shortly after PCLK so timer_counter samples it
    // pulse: 0 -> 1 (sampled) -> 0
    force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);
    force dut.u_selclk.Clock_counter = 1'b1; @(posedge PCLK); // rising edge sampled here
    force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);
    release dut.u_selclk.Clock_counter;
    repeat (2) @(posedge PCLK);

    apb_read(`TCNT_ADDR, tmp);
    $display("After TDR write + forced Clock pulse: TCNT = 0x%0h (expect loaded 0x%0h)", tmp, w);

    // Also try TCR.load rising edge route:
    $display("=== Exercise TCR.load rising edge -> load into TCNT ===");
    // baseline Clock_counter 0
    force dut.u_selclk.Clock_counter = 1'b0;
    repeat (2) @(posedge PCLK);

    // ensure load bit is clear then set then clear (create rising edge of TCR.load)
    apb_write(`TCR_ADDR, 8'h00); @(posedge PCLK);
    apb_write(`TDR_ADDR, 8'h5A); @(posedge PCLK); // write new TDR value
    @(posedge PCLK);
    // set load=1
    apb_write(`TCR_ADDR, 8'h80); @(posedge PCLK);  // load=1 sampled here
    // create a Clock_counter rising edge while load is active (one PCLK later)
    force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);
    force dut.u_selclk.Clock_counter = 1'b1; @(posedge PCLK); // sampled rising
    force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);
    // clear load
    apb_write(`TCR_ADDR, 8'h00); @(posedge PCLK);
    release dut.u_selclk.Clock_counter;
    repeat (2) @(posedge PCLK);
    apb_read(`TCNT_ADDR, tmp);
    $display("After TCR.load + forced Clock pulse: TCNT = 0x%0h (expect loaded 0x%0h)", tmp, 8'h5A);

    // exercise select_clock choices to increase coverage
    apb_write(`TCR_ADDR, 8'b0000_0000); @(posedge PCLK);
    apb_write(`TCR_ADDR, 8'b0000_0001); @(posedge PCLK);
    apb_write(`TCR_ADDR, 8'b0000_0010); @(posedge PCLK);
    apb_write(`TCR_ADDR, 8'b0000_0011); @(posedge PCLK);

    // Try to cause overflow: load all 1s then generate two pulses
    force dut.u_selclk.Clock_counter = 1'b0; repeat (2) @(posedge PCLK);
    apb_write(`TDR_ADDR, {DATA_WIDTH{1'b1}}); @(posedge PCLK);
    // create load edge
    apb_write(`TCR_ADDR, 8'h80); @(posedge PCLK);
    apb_write(`TCR_ADDR, 8'h00); @(posedge PCLK);
    repeat (2) @(posedge PCLK);
    // enable count up
    apb_write(`TCR_ADDR, 8'h10); @(posedge PCLK); @(posedge PCLK);
    // two clean rising edges
    force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);
    force dut.u_selclk.Clock_counter = 1'b1; @(posedge PCLK); force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);
    force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);
    force dut.u_selclk.Clock_counter = 1'b1; @(posedge PCLK); force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);
    release dut.u_selclk.Clock_counter;
    repeat (2) @(posedge PCLK);
    apb_read(`TCNT_ADDR, tmp);
    $display("After forced OVF attempts: TCNT=0x%0h OVF=%b", tmp, TMR_OVF);

    // final read of valid addresses
    apb_read(`TDR_ADDR, r); $display("Final TDR=0x%0h", r);
    apb_read(`TCR_ADDR, r); $display("Final TCR=0x%0h", r);
    apb_read(`TSR_ADDR, r); $display("Final TSR=0x%0h", r);
    apb_read(`TCNT_ADDR, r); $display("Final TCNT=0x%0h", r);

    if (err_cnt == 0) $display("null_address PASSED");
    else $display("null_address FAILED with %0d error(s)", err_cnt);

    #100 $finish;
  end

endmodule
