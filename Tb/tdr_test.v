`timescale 1ns/1ps
module tdr_test;
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
  wire                     TMR_OVF, TMR_UDF;

  // DUT instance (adjust parameters if needed)
  timer_top #(.ADDR_WIDTH(ADDR_WIDTH), .DATA_WIDTH(DATA_WIDTH)) dut (
    .PCLK(PCLK), .PRESET_n(PRESET_n),
    .PSEL(PSEL), .PWRITE(PWRITE), .PENABLE(PENABLE),
    .PADDR(PADDR), .PWDATA(PWDATA),
    .PRDATA(PRDATA), .PREADY(PREADY), .PSLVERR(PSLVERR),
    .TMR_OVF(TMR_OVF), .TMR_URF(TMR_UDF) // keep names if your top port names differ
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

  // APB write (blocking assigns inside TB)
  task apb_write(input [ADDR_WIDTH-1:0] addr, input [DATA_WIDTH-1:0] data_in);
    begin
      @(posedge PCLK);
      $display("[%0t] TB: APB WRITE req addr=0x%0h data=0x%0h", $time, addr, data_in);
      PADDR = addr; PWDATA = data_in; PWRITE = 1'b1; PSEL = 1'b1; PENABLE = 1'b0;
      @(posedge PCLK);
      PENABLE = 1'b1;
      while (!PREADY) @(posedge PCLK);
      @(posedge PCLK);
      PSEL = 1'b0; PWRITE = 1'b0; PENABLE = 1'b0;
      PADDR = {ADDR_WIDTH{1'b0}}; PWDATA = {DATA_WIDTH{1'b0}};
    end
  endtask

  // APB write that forces internal PREADY=0 for wait_cycles (cover PREADY_0 branch)
  task apb_write_with_forced_wait;
    input [ADDR_WIDTH-1:0] addr;
    input [DATA_WIDTH-1:0] data_in;
    input integer wait_cycles;
    integer j;
    begin
      @(posedge PCLK);
      $display("[%0t] TB: APB WRITE forced-wait req addr=0x%0h data=0x%0h wait=%0d", $time, addr, data_in, wait_cycles);
      PADDR = addr; PWDATA = data_in; PWRITE = 1'b1; PSEL = 1'b1; PENABLE = 1'b0;
      @(posedge PCLK);
      PENABLE = 1'b1;

      // Force internal PREADY low for wait_cycles clocks to simulate wait-state.
      // NOTE: adjust this hierarchical path to your APB slave instance inside timer_top
      force dut.u_apb.PREADY = 1'b0;
      for (j = 0; j < wait_cycles; j = j + 1) @(posedge PCLK);
      release dut.u_apb.PREADY;

      // wait until DUT asserts PREADY, finish transaction
      while (!PREADY) @(posedge PCLK);
      @(posedge PCLK);
      PSEL = 1'b0; PWRITE = 1'b0; PENABLE = 1'b0;
      PADDR = {ADDR_WIDTH{1'b0}}; PWDATA = {DATA_WIDTH{1'b0}};
    end
  endtask

  // APB read (blocking assigns)
  task apb_read(input [ADDR_WIDTH-1:0] addr, output [DATA_WIDTH-1:0] data_out);
    begin
      @(posedge PCLK);
      $display("[%0t] TB: APB READ req addr=0x%0h", $time, addr);
      PADDR = addr; PWRITE = 1'b0; PSEL = 1'b1; PENABLE = 1'b0;
      @(posedge PCLK);
      PENABLE = 1'b1;
      while (!PREADY) @(posedge PCLK);
      data_out = PRDATA;
      @(posedge PCLK);
      PSEL = 1'b0; PENABLE = 1'b0;
      PADDR = {ADDR_WIDTH{1'b0}};
      $display("[%0t] TB: APB READ rdata=0x%0h", $time, data_out);
    end
  endtask

  // APB read with forced wait-state
  task apb_read_with_forced_wait;
    input  [ADDR_WIDTH-1:0] addr;
    output [DATA_WIDTH-1:0] data_out;
    input  integer         wait_cycles;
    integer j;
    begin
      @(posedge PCLK);
      $display("[%0t] TB: APB READ forced-wait req addr=0x%0h wait=%0d", $time, addr, wait_cycles);
      PADDR = addr; PWRITE = 1'b0; PSEL = 1'b1; PENABLE = 1'b0;
      @(posedge PCLK);
      PENABLE = 1'b1;

      force dut.u_apb.PREADY = 1'b0;
      for (j = 0; j < wait_cycles; j = j + 1) @(posedge PCLK);
      release dut.u_apb.PREADY;

      while (!PREADY) @(posedge PCLK);
      data_out = PRDATA;
      @(posedge PCLK);
      PSEL = 1'b0; PENABLE = 1'b0;
      PADDR = {ADDR_WIDTH{1'b0}};
      $display("[%0t] TB: APB READ forced-wait rdata=0x%0h", $time, data_out);
    end
  endtask

  // random function
  function [DATA_WIDTH-1:0] rand8; input dummy; begin rand8 = $random; end endfunction

  // ---------- helpers to force internal clocks/inputs ----------
  // Force N clean rising edges on the Clock_counter output of select_clock
  task force_clock_rise_n;
    input integer n;
    integer i;
    begin
      if (n <= 0) disable force_clock_rise_n;
      // ensure prev_clk inside u_cnt is zero so rising edge detection works
      // (force prev_clk = 0 if path exists)
      // NOTE: if your u_cnt uses a different name for prev_clk change it
      force dut.u_cnt.prev_clk = 1'b0;
      for (i = 0; i < n; i = i + 1) begin
        force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);
        force dut.u_selclk.Clock_counter = 1'b1; @(posedge PCLK); // rising edge sampled here
        force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);
      end
      release dut.u_selclk.Clock_counter;
      release dut.u_cnt.prev_clk;
      repeat (2) @(posedge PCLK);
    end
  endtask

  // Force different Clk_in patterns and write TCR.CKS so select_clock uses them
  task cover_select_clock_patterns;
    integer ci;
    reg [DATA_WIDTH-1:0] tmp_rd;
    begin
      $display("--- cover_select_clock_patterns ---");
      for (ci = 0; ci < 4; ci = ci + 1) begin
        // set CKS in TCR so select_clock will pick that bit
        apb_write(`TCR_ADDR, ( (ci << `TCR_Cks_0) & (2'b11 << `TCR_Cks_0) )); // simpler write, may mask if your make_tcr is needed
        repeat (2) @(posedge PCLK);

        // force Clk_in all zeros then selected bit = 1
        force dut.u_selclk.Clk_in = 4'h0; @(posedge PCLK);
        apb_read(`TCNT_ADDR, tmp_rd); // create bus activity
        force dut.u_selclk.Clk_in = (4'h1 << ci); @(posedge PCLK);
        apb_read(`TCNT_ADDR, tmp_rd);
        release dut.u_selclk.Clk_in;
        repeat (2) @(posedge PCLK);

        $display("  CKS=%0d covered by forcing Clk_in bit %0d", ci, ci);
      end
    end
  endtask

  // clear both TSR bits (OVF,UDF) – write-one-to-clear
  task clear_tsr_all;
    reg [DATA_WIDTH-1:0] tmp; reg e;
    begin
      tmp = (1<<`TMR_OVF_bit) | (1<<`TMR_UDF_bit);
      apb_write(`TSR_ADDR, tmp);
      repeat (2) @(posedge PCLK);
      apb_read(`TSR_ADDR, tmp);
      $display("  [clear_tsr_all] TSR=0x%0h", tmp);
    end
  endtask

  integer i;
  integer err_cnt;
  reg [DATA_WIDTH-1:0] r, w, tmp;
  reg [ADDR_WIDTH-1:0] addr;

  initial begin
    err_cnt = 0;

    // 1) reset & read default TDR/TCR/TSR/TCNT
    do_reset();
    apb_read(`TDR_ADDR, r); $display("TDR default = 0x%0h", r);
    apb_read(`TCR_ADDR, r); $display("TCR default = 0x%0h", r);
    apb_read(`TSR_ADDR, r); $display("TSR default = 0x%0h", r);
    apb_read(`TCNT_ADDR, r); $display("TCNT default = 0x%0h", r);

    // 2) TDR loop
    for (i = 0; i < LOOPS; i = i + 1) begin
      w = rand8(0);
      apb_write(`TDR_ADDR, w);
      apb_read (`TDR_ADDR, r);
      if (r !== w) begin
        $display("[%0t] ERROR: TDR mismatch i=%0d w=0x%0h r=0x%0h", $time, i, w, r);
        err_cnt = err_cnt + 1;
      end
    end
    $display("TDR loop done");

    // 3) Write to TCR
    w = rand8(0);
    apb_write(`TCR_ADDR, w);
    apb_read(`TCR_ADDR, r);
    $display("After TCR write wrote=0x%0h read=0x%0h", w, r);

    // 4) Write to TSR
    w = rand8(0);
    apb_write(`TSR_ADDR, w);
    apb_read(`TSR_ADDR, r);
    $display("After TSR write wrote=0x%0h read=0x%0h", w, r);

    // 5) invalid write -> PSLVERR
    addr = 8'h5A; w = rand8(0);
    apb_write(addr, w);
    if (PSLVERR) $display("Invalid write to 0x%0h => PSLVERR asserted (OK)", addr);
    else begin $display("Invalid write to 0x%0h => PSLVERR NOT asserted (unexpected)", addr); err_cnt = err_cnt + 1; end

    // 6) Force a write with wait-state
    w = rand8(0);
    apb_write_with_forced_wait(`TDR_ADDR, w, 3);
    apb_read(`TDR_ADDR, r);
    $display("After forced-wait write, TDR read=0x%0h", r);

    // 7) Force a read with wait-state
    apb_read_with_forced_wait(`TDR_ADDR, r, 3);

    // ==================== OVF / UDF exercise block (improved timing) ====================
    $display("=== OVF sequence start (improved) ===");

    // ensure Clock_counter baseline 0 and prev_clk = 0
    force dut.u_selclk.Clock_counter = 1'b0;
    force dut.u_cnt.prev_clk = 1'b0;
    repeat (2) @(posedge PCLK);

    // load TDR = all 1's and create load edge
    tmp = {DATA_WIDTH{1'b1}};
    apb_write(`TDR_ADDR, tmp); @(posedge PCLK);
    apb_write(`TCR_ADDR, 8'h00); @(posedge PCLK);
    apb_write(`TCR_ADDR, 8'h80); @(posedge PCLK); // load=1
    apb_write(`TCR_ADDR, 8'h00); @(posedge PCLK); // load=0
    repeat (3) @(posedge PCLK);

    apb_read(`TCNT_ADDR, tmp);
    $display("DBG: After load, TCNT = 0x%0h (expect all 1s)", tmp);
    $display("DBG_INTERNAL: dut.u_cnt.TCNT_Out=%0h, dut.u_selclk.Clock_counter=%b", dut.u_cnt.TCNT_Out, dut.u_selclk.Clock_counter);

    // enable count up
    apb_write(`TCR_ADDR, 8'h10); @(posedge PCLK);
    repeat (1) @(posedge PCLK);

    // force two clean rising edges using helper (guaranteed sampling)
    $display("Forcing two clean Clock_counter rising edges (OVF) ...");
    force_clock_rise_n(2);

    apb_read(`TCNT_ADDR, tmp);
    $display("After forced OVF pulses: TCNT = 0x%0h, TMR_OVF=%b (internal TCNT=%0h)",
             tmp, TMR_OVF, dut.u_cnt.TCNT_Out);

    // Underflow
    $display("=== UDF sequence start (improved) ===");
    force dut.u_selclk.Clock_counter = 1'b0;
    force dut.u_cnt.prev_clk = 1'b0;
    repeat (2) @(posedge PCLK);

    tmp = {DATA_WIDTH{1'b0}};
    apb_write(`TDR_ADDR, tmp); @(posedge PCLK);
    apb_write(`TCR_ADDR, 8'h00); @(posedge PCLK);
    apb_write(`TCR_ADDR, 8'h80); @(posedge PCLK);
    apb_write(`TCR_ADDR, 8'h00); @(posedge PCLK);
    repeat (3) @(posedge PCLK);

    apb_write(`TCR_ADDR, 8'h30); @(posedge PCLK); // enable down
    repeat (1) @(posedge PCLK);

    apb_read(`TCNT_ADDR, tmp);
    $display("Before UDF pulses: TCNT = 0x%0h (expect 0x00)", tmp);

    $display("Forcing two clean Clock_counter rising edges (UDF) ...");
    force_clock_rise_n(2);

    apb_read(`TCNT_ADDR, tmp);
    $display("After forced UDF pulses: TCNT = 0x%0h, TMR_UDF=%b (internal TCNT=%0h)",
             tmp, TMR_UDF, dut.u_cnt.TCNT_Out);

    // release any remaining forces
    release dut.u_selclk.Clock_counter;
    release dut.u_cnt.prev_clk;
    release dut.u_selclk.Clk_in;

    // 8) Extra coverage: cycle CKS + force Clk_in patterns
    cover_select_clock_patterns();

    // 9) Read all valid addresses
    apb_read(`TDR_ADDR, r); $display("Read TDR = 0x%0h", r);
    apb_read(`TCR_ADDR, r); $display("Read TCR = 0x%0h", r);
    apb_read(`TSR_ADDR, r); $display("Read TSR = 0x%0h", r);
    apb_read(`TCNT_ADDR, r); $display("Read TCNT= 0x%0h", r);

    if (err_cnt == 0) $display("tdr_test (extended) PASSED");
    else $display("tdr_test (extended) FAILED with %0d error(s)", err_cnt);

    #100 $finish;
  end
endmodule
