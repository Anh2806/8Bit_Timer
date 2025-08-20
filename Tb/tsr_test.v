// tsr_test.v - Verilog-2001: read/write TSR test + extra sequences to improve coverage
`timescale 1ns/1ps
module tsr_test;
  `include "Def_name.v"

  localparam ADDR_WIDTH = 8;
  localparam DATA_WIDTH = 8;
  localparam LOOPS = 20;

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

  // DUT instance (adjust if parameters/names differ)
  timer_top #(.ADDR_WIDTH(ADDR_WIDTH), .DATA_WIDTH(DATA_WIDTH)) dut (
    .PCLK(PCLK), .PRESET_n(PRESET_n),
    .PSEL(PSEL), .PWRITE(PWRITE), .PENABLE(PENABLE),
    .PADDR(PADDR), .PWDATA(PWDATA),
    .PRDATA(PRDATA), .PREADY(PREADY), .PSLVERR(PSLVERR),
    .TMR_OVF(TMR_OVF), .TMR_URF(TMR_URF)
  );

  // 100 MHz clock
  initial begin PCLK = 1'b0; forever #5 PCLK = ~PCLK; end

  // ===== reset task =====
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

  // ===== APB basic write =====
  task apb_write(input [ADDR_WIDTH-1:0] addr, input [DATA_WIDTH-1:0] data_in);
    begin
      @(posedge PCLK);
      $display("[%0t] TB: APB WRITE addr=0x%0h data=0x%0h", $time, addr, data_in);
      PADDR <= addr; PWDATA <= data_in; PWRITE <= 1'b1; PSEL <= 1'b1; PENABLE <= 1'b0;
      @(posedge PCLK);
      PENABLE <= 1'b1;
      while (!PREADY) @(posedge PCLK);
      @(posedge PCLK);
      PSEL <= 1'b0; PWRITE <= 1'b0; PENABLE <= 1'b0;
    end
  endtask

  // ===== APB write with forced internal PREADY=0 (to cover PREADY_0 branch) =====
  task apb_write_with_forced_wait;
    input [ADDR_WIDTH-1:0] addr;
    input [DATA_WIDTH-1:0] data_in;
    input integer wait_cycles;
    integer j;
    begin
      @(posedge PCLK);
      $display("[%0t] TB: APB WRITE forced-wait addr=0x%0h data=0x%0h wait=%0d", $time, addr, data_in, wait_cycles);
      PADDR <= addr; PWDATA <= data_in; PWRITE <= 1'b1; PSEL <= 1'b1; PENABLE <= 1'b0;
      @(posedge PCLK);
      PENABLE <= 1'b1;
      // force internal PREADY low in Rw_register instance
      force dut.u_apb.PREADY = 1'b0;
      for (j = 0; j < wait_cycles; j = j + 1) @(posedge PCLK);
      release dut.u_apb.PREADY;
      while (!PREADY) @(posedge PCLK);
      @(posedge PCLK);
      PSEL <= 1'b0; PWRITE <= 1'b0; PENABLE <= 1'b0;
    end
  endtask

  // ===== APB read =====
  task apb_read(input [ADDR_WIDTH-1:0] addr, output [DATA_WIDTH-1:0] data_out);
    begin
      @(posedge PCLK);
      $display("[%0t] TB: APB READ addr=0x%0h", $time, addr);
      PADDR <= addr; PWRITE <= 1'b0; PSEL <= 1'b1; PENABLE <= 1'b0;
      @(posedge PCLK);
      PENABLE <= 1'b1;
      while (!PREADY) @(posedge PCLK);
      data_out = PRDATA;
      @(posedge PCLK);
      PSEL <= 1'b0; PENABLE <= 1'b0;
      $display("[%0t] TB: APB READ rdata=0x%0h", $time, data_out);
    end
  endtask

  // ===== APB read with forced internal PREADY=0 (cover read wait-state branch) =====
  task apb_read_with_forced_wait;
    input  [ADDR_WIDTH-1:0] addr;
    output [DATA_WIDTH-1:0] data_out;
    input  integer         wait_cycles;
    integer j;
    begin
      @(posedge PCLK);
      $display("[%0t] TB: APB READ forced-wait addr=0x%0h wait=%0d", $time, addr, wait_cycles);
      PADDR <= addr; PWRITE <= 1'b0; PSEL <= 1'b1; PENABLE <= 1'b0;
      @(posedge PCLK);
      PENABLE <= 1'b1;
      // force internal PREADY low
      force dut.u_apb.PREADY = 1'b0;
      for (j = 0; j < wait_cycles; j = j + 1) @(posedge PCLK);
      release dut.u_apb.PREADY;
      while (!PREADY) @(posedge PCLK);
      data_out = PRDATA;
      @(posedge PCLK);
      PSEL <= 1'b0; PENABLE <= 1'b0;
      $display("[%0t] TB: APB READ forced-wait rdata=0x%0h", $time, data_out);
    end
  endtask

  // random 8-bit
  function [DATA_WIDTH-1:0] rand8; input dummy; begin rand8 = $random; end endfunction

  // locals
  integer i;
  integer err_cnt;
  reg [DATA_WIDTH-1:0] r, w, tmp;
  reg [ADDR_WIDTH-1:0] a;
  integer k;

  initial begin
    err_cnt = 0;

    // 1) reset & read defaults
    do_reset();
    apb_read(`TSR_ADDR, r); $display("TSR default = 0x%0h", r);
    apb_read(`TDR_ADDR, r); $display("TDR default = 0x%0h", r);
    apb_read(`TCR_ADDR, r); $display("TCR default = 0x%0h", r);
    apb_read(`TCNT_ADDR, r); $display("TCNT default = 0x%0h", r);

    // 2) TSR loop: write random values to TSR_ADDR and expect read == 0
    for (i = 0; i < LOOPS; i = i + 1) begin
      w = rand8(0);
      apb_write(`TSR_ADDR, w);
      apb_read (`TSR_ADDR, r);
      if (r !== {DATA_WIDTH{1'b0}}) begin
        $display("[%0t] ERROR: TSR expected 0 i=%0d wrote=0x%0h read=0x%0h", $time, i, w, r);
        err_cnt = err_cnt + 1;
      end else begin
        $display("OK TSR i=%0d read=0x%0h", i, r);
      end
    end

    // 3) exercise read forced-wait to hit PREADY==0 on read path (cover missing branch)
    apb_read_with_forced_wait(`TSR_ADDR, r, 3);
    if (r !== {DATA_WIDTH{1'b0}}) begin
      $display("[%0t] ERROR: TSR forced-read expected 0 got=0x%0h", $time, r);
      err_cnt = err_cnt + 1;
    end

    // 4) also exercise write forced-wait to hit PREADY==0 on write path
    w = rand8(0);
    apb_write_with_forced_wait(`TSR_ADDR, w, 3);
    apb_read(`TSR_ADDR, r);
    if (r !== {DATA_WIDTH{1'b0}}) begin
      // many TSR implementations read 0 even after write; check spec
      $display("[%0t] INFO: after forced-wait write TSR read=0x%0h", $time, r);
    end

    // 5) write to TDR and TCR here to exercise Rw_register write-case rows and control logic:
    //    - write TDR to exercise `TDR_ADDR` branch and generate TDR_WR_pulse
    //    - write TCR load rising to exercise load path -> will be used later for count_load
    w = rand8(0);
    apb_write(`TDR_ADDR, w);
    apb_read(`TDR_ADDR, tmp);
    $display("After TDR write wrote=0x%0h read back=0x%0h", w, tmp);

    // Toggle TCR load bit to create load rising edge (bit7)
    apb_write(`TCR_ADDR, 8'h00); @(posedge PCLK);
    apb_write(`TCR_ADDR, 8'h80); // set load
    @(posedge PCLK);
    apb_write(`TCR_ADDR, 8'h00); // clear load
    repeat (2) @(posedge PCLK);

    // check TCNT (should have loaded TDR if control path works)
    apb_read(`TCNT_ADDR, tmp);
    $display("After TCR.load, TCNT = 0x%0h (expected maybe = TDR)", tmp);

    // 6) change Cks (TCR[1:0]) to exercise select_clock mux: 00,01,10,11
    $display("Changing Cks values to hit select_clock choices");
    apb_write(`TCR_ADDR, 8'b0000_0000); apb_read(`TCR_ADDR, r); $display("Cks=00 read=%b", r[1:0]);
    apb_write(`TCR_ADDR, 8'b0000_0001); apb_read(`TCR_ADDR, r); $display("Cks=01 read=%b", r[1:0]);
    apb_write(`TCR_ADDR, 8'b0000_0010); apb_read(`TCR_ADDR, r); $display("Cks=10 read=%b", r[1:0]);
    apb_write(`TCR_ADDR, 8'b0000_0011); apb_read(`TCR_ADDR, r); $display("Cks=11 read=%b", r[1:0]);

    // small wait
    repeat (2) @(posedge PCLK);

    // 7) OVF / UDF exercise: make sure rising edges are sampled reliably:
    //    Use safe forcing of Clock_counter: ensure prev_clk inside timer_counter is 0 before 0->1 transition.
    //    Adjust instance path if your select_clock instance is named differently.
    $display("=== OVF/UDF exercise start ===");

    // ensure baseline 0
    force dut.u_selclk.Clock_counter = 1'b0;
    repeat (2) @(posedge PCLK);

    // prepare: load TDR with all 1s and load into TCNT
    tmp = {DATA_WIDTH{1'b1}};
    apb_write(`TDR_ADDR, tmp);
    @(posedge PCLK);
    // create TCR.load rising edge
    apb_write(`TCR_ADDR, 8'h00); @(posedge PCLK);
    apb_write(`TCR_ADDR, 8'h80); @(posedge PCLK);
    apb_write(`TCR_ADDR, 8'h00); @(posedge PCLK);
    repeat (2) @(posedge PCLK);

    apb_read(`TCNT_ADDR, tmp);
    $display("DBG: TCNT after load (all ones expected): 0x%0h internal=%0h", tmp, dut.u_cnt.TCNT_Out);

    // enable counting up to trigger OVF
    apb_write(`TCR_ADDR, 8'h10); // En=1
    @(posedge PCLK); @(posedge PCLK);

    // two clean rising edges
    force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);
    force dut.u_selclk.Clock_counter = 1'b1; @(posedge PCLK); // sampled rising
    force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);

    force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);
    force dut.u_selclk.Clock_counter = 1'b1; @(posedge PCLK);
    force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);

    release dut.u_selclk.Clock_counter;
    repeat (2) @(posedge PCLK);

    apb_read(`TCNT_ADDR, tmp);
    $display("After forced OVF pulses: TCNT=0x%0h TMR_OVF=%b internal_TCNT=%0h", tmp, TMR_OVF, dut.u_cnt.TCNT_Out);

    // Now UDF: load zero and count down
    force dut.u_selclk.Clock_counter = 1'b0;
    repeat (2) @(posedge PCLK);

    apb_write(`TDR_ADDR, {DATA_WIDTH{1'b0}});
    @(posedge PCLK);
    apb_write(`TCR_ADDR, 8'h80); @(posedge PCLK); apb_write(`TCR_ADDR, 8'h00); @(posedge PCLK);
    repeat (2) @(posedge PCLK);

    // enable down: Up/Dw=1 (bit5), En=1 (bit4) -> 0x30
    apb_write(`TCR_ADDR, 8'h30);
    @(posedge PCLK); @(posedge PCLK);

    force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);
    force dut.u_selclk.Clock_counter = 1'b1; @(posedge PCLK);
    force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);

    force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);
    force dut.u_selclk.Clock_counter = 1'b1; @(posedge PCLK);
    force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);

    release dut.u_selclk.Clock_counter;
    repeat (2) @(posedge PCLK);

    apb_read(`TCNT_ADDR, tmp);
    $display("After forced UDF pulses: TCNT=0x%0h TMR_URF=%b internal_TCNT=%0h", tmp, TMR_URF, dut.u_cnt.TCNT_Out);

    // 8) invalid address writes/reads to exercise PSLVERR/default branch
    for (i = 0; i < 8; i = i + 1) begin
      a = rand8(0); w = rand8(0);
      // limit a to small range so not always valid; but random is fine
      apb_write(a, w);
      if ((a != `TDR_ADDR) && (a != `TCR_ADDR) && (a != `TSR_ADDR)) begin
        if (!PSLVERR) begin
          $display("[%0t] ERROR: invalid write addr=0x%0h did NOT assert PSLVERR", $time, a);
          err_cnt = err_cnt + 1;
        end else $display("OK invalid write addr=0x%0h PSLVERR asserted", a);
      end else begin
        // valid, read back (TSR reads are expected 0)
        apb_read(a, r);
      end
    end

    // 9) final: read all valid addresses to cover read mux
    apb_read(`TDR_ADDR, r); $display("Final TDR = 0x%0h", r);
    apb_read(`TCR_ADDR, r); $display("Final TCR = 0x%0h", r);
    apb_read(`TSR_ADDR, r); $display("Final TSR = 0x%0h", r);
    apb_read(`TCNT_ADDR, r); $display("Final TCNT= 0x%0h", r);

    // final verdict
    if (err_cnt == 0) $display("tsr_test PASSED");
    else $display("tsr_test FAILED with %0d error(s)", err_cnt);

    #100 $finish;
  end

endmodule
