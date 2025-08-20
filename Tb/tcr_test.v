// tcr_test.v - Verilog-2001 only. Focus on TCR.
// Tests: reset value, masked R/W (20x), PSLVERR on invalid addr, read all valid addrs.
// Added: sequences to exercise TDR_WR_pulse and TCR.load rising edge, and Overflow/Underflow cases.
`timescale 1ns/1ps
`ifndef DATA_WIDTH
  `define DATA_WIDTH 8
`endif

module tcr_test;
  `include "Def_name.v"

  // Parameters
  localparam ADDR_WIDTH = 8;
  localparam DATA_WIDTH = 8;

  // Writable bits in TCR: [7]Load, [5]Up/Dw, [4]En, [1:0]Cks
  localparam [DATA_WIDTH-1:0] TCR_MASK = 8'b1011_0011;

  // APB signals
  reg                       PCLK;
  reg                       PRESET_n;
  reg                       PSEL;
  reg                       PWRITE;
  reg                       PENABLE;
  reg   [ADDR_WIDTH-1:0]    PADDR;
  reg   [DATA_WIDTH-1:0]    PWDATA;
  wire  [DATA_WIDTH-1:0]    PRDATA;
  wire                      PREADY;
  wire                      PSLVERR;
  wire                      TMR_OVF, TMR_URF;

  // DUT
  timer_top #(.ADDR_WIDTH(ADDR_WIDTH), .DATA_WIDTH(DATA_WIDTH)) dut (
    .PCLK     (PCLK),
    .PRESET_n (PRESET_n),
    .PSEL     (PSEL),
    .PWRITE   (PWRITE),
    .PENABLE  (PENABLE),
    .PADDR    (PADDR),
    .PWDATA   (PWDATA),
    .PRDATA   (PRDATA),
    .PREADY   (PREADY),
    .PSLVERR  (PSLVERR),
    .TMR_OVF  (TMR_OVF),
    .TMR_URF  (TMR_URF)
  );

  // 100 MHz clk
  initial begin
    PCLK = 1'b0;
    forever #5 PCLK = ~PCLK;
  end

  // debug: print APB signals every cycle to observe handshake
  always @(posedge PCLK) begin
    $display("%0t TB: PSEL=%b PWRITE=%b PEN=%b PREADY=%b PSLVERR=%b PADDR=0x%0h PWDATA=0x%0h PRDATA=0x%0h",
             $time, PSEL, PWRITE, PENABLE, PREADY, PSLVERR, PADDR, PWDATA, PRDATA);
  end

  // reset task
  task do_reset;
    begin
      PRESET_n = 1'b0;
      PSEL     = 1'b0;
      PWRITE   = 1'b0;
      PENABLE  = 1'b0;
      PADDR    = {ADDR_WIDTH{1'b0}};
      PWDATA   = {DATA_WIDTH{1'b0}};
      repeat (5) @(posedge PCLK);
      PRESET_n = 1'b1;
      repeat (2) @(posedge PCLK);
    end
  endtask

  // APB write
  task apb_write(input [ADDR_WIDTH-1:0] addr,
                 input [DATA_WIDTH-1:0] data_in);
    begin
      @(posedge PCLK);
      PADDR   <= addr;
      PWDATA  <= data_in;
      PWRITE  <= 1'b1;
      PSEL    <= 1'b1;
      PENABLE <= 1'b0;  // SETUP
      @(posedge PCLK);
      PENABLE <= 1'b1;  // ACCESS
      while (!PREADY) @(posedge PCLK);
      @(posedge PCLK);  // return to IDLE
      PSEL    <= 1'b0;
      PWRITE  <= 1'b0;
      PENABLE <= 1'b0;
    end
  endtask

  // APB write with forced wait-state (forces internal PREADY=0 for n cycles)
  task apb_write_with_forced_wait;
    input [ADDR_WIDTH-1:0] addr;
    input [DATA_WIDTH-1:0] data_in;
    input integer wait_cycles;
    integer j;
    begin
      @(posedge PCLK);
      PADDR   <= addr;
      PWDATA  <= data_in;
      PWRITE  <= 1'b1;
      PSEL    <= 1'b1;
      PENABLE <= 1'b0; // SETUP
      @(posedge PCLK);
      PENABLE <= 1'b1; // ACCESS

      // Force internal PREADY low for wait_cycles clocks to simulate wait-state.
      // Path assumes Rw_register instance is u_apb inside timer_top.
      force dut.u_apb.PREADY = 1'b0;
      for (j = 0; j < wait_cycles; j = j + 1) begin
        @(posedge PCLK);
      end
      release dut.u_apb.PREADY;

      // wait until DUT asserts PREADY (normal behavior); then complete transfer
      while (!PREADY) @(posedge PCLK);
      @(posedge PCLK);
      PSEL   <= 1'b0;
      PWRITE <= 1'b0;
      PENABLE<= 1'b0;
    end
  endtask

  // APB read
  task apb_read(input  [ADDR_WIDTH-1:0] addr,
                output [DATA_WIDTH-1:0] data_out);
    begin
      @(posedge PCLK);
      PADDR   <= addr;
      PWRITE  <= 1'b0;
      PSEL    <= 1'b1;
      PENABLE <= 1'b0;  // SETUP
      @(posedge PCLK);
      PENABLE <= 1'b1;  // ACCESS
      while (!PREADY) @(posedge PCLK);
      data_out = PRDATA;
      @(posedge PCLK);  // IDLE
      PSEL    <= 1'b0;
      PENABLE <= 1'b0;
    end
  endtask

  // 8-bit random
  function [DATA_WIDTH-1:0] rand8;
    input dummy;
    begin
      rand8 = $random;
    end
  endfunction

  // Locals
  reg [DATA_WIDTH-1:0] r, w;
  integer i;
  integer k;
  integer err_cnt;

  initial begin
    err_cnt = 0;

    // 1) Reset & read default TCR
    do_reset();
    apb_read(`TCR_ADDR, r);
`ifdef TCR_RST
    if ((r & TCR_MASK) !== (`TCR_RST & TCR_MASK)) begin
      $display("[%0t] ERROR: TCR reset mismatch got=%0h exp=%0h", $time, r, `TCR_RST);
      err_cnt = err_cnt + 1;
    end else $display("OK: TCR reset = %0h", r);
`else
    $display("INFO: TCR after reset = %0h (no TCR_RST macro to compare)", r);
`endif

    // 2) Masked R/W 20x
    do_reset();
    for (i = 0; i < 20; i = i + 1) begin
      w = rand8(0);
      apb_write(`TCR_ADDR, w);
      apb_read (`TCR_ADDR, r);
      if ((r & TCR_MASK) !== (w & TCR_MASK)) begin
        $display("[%0t] ERROR: masked RW miscompare i=%0d w=%0h r=%0h mask=%0h",
                 $time, i, w, r, TCR_MASK);
        err_cnt = err_cnt + 1;
      end
      // Reserved bits must read 0 (bits 6,3,2)
      if (r[6] || r[3] || r[2]) begin
        $display("[%0t] ERROR: reserved bits not zero r=%0h", $time, r);
        err_cnt = err_cnt + 1;
      end
    end
    $display("OK: Masked R/W loop complete");

    // -------------------------------------------------------------------------
    // EXTRA: exercise PREADY == 0 (APB wait-state) to hit that condition in Rw_register
    // -------------------------------------------------------------------------
    // Example: force 3 cycles of wait-state
    apb_write_with_forced_wait(`TCR_ADDR, 8'h5A, 3);
    repeat (2) @(posedge PCLK);

    // 3) PSLVERR: valid vs invalid address
    do_reset();
    apb_write(`TCR_ADDR, 8'hAA);
    if (PSLVERR) begin
      $display("[%0t] ERROR: PSLVERR asserted on valid addr", $time);
      err_cnt = err_cnt + 1;
    end
    // invalid address (outside 0x00..0x03)
    apb_write(8'h55, 8'hAA);
    if (!PSLVERR) begin
      $display("[%0t] ERROR: PSLVERR not asserted on invalid addr", $time);
      err_cnt = err_cnt + 1;
    end

    // 5) Enumerate Cks = 00/01/10/11 to exercise select_clock paths
    do_reset();
    apb_write(`TCR_ADDR, 8'b0000_0000); apb_read(`TCR_ADDR, r);
    if (r[1:0] !== 2'b00) begin $display("ERROR: Cks set 00 read %b", r[1:0]); err_cnt = err_cnt + 1; end
    apb_write(`TCR_ADDR, 8'b0000_0001); apb_read(`TCR_ADDR, r);
    if (r[1:0] !== 2'b01) begin $display("ERROR: Cks set 01 read %b", r[1:0]); err_cnt = err_cnt + 1; end
    apb_write(`TCR_ADDR, 8'b0000_0010); apb_read(`TCR_ADDR, r);
    if (r[1:0] !== 2'b10) begin $display("ERROR: Cks set 10 read %b", r[1:0]); err_cnt = err_cnt + 1; end
    apb_write(`TCR_ADDR, 8'b0000_0011); apb_read(`TCR_ADDR, r);
    if (r[1:0] !== 2'b11) begin $display("ERROR: Cks set 11 read %b", r[1:0]); err_cnt = err_cnt + 1; end

    // -------------------------------------------------------------------------
    // Sequences to exercise Load_Tdr and TDR_WR_pulse paths
    // -------------------------------------------------------------------------
    do_reset();

    // Ensure load bit = 0
    apb_write(`TCR_ADDR, 8'h00);
    @(posedge PCLK);

    // Write TDR (when load=0) to create TDR_WR_pulse path
    apb_write(`TDR_ADDR, 8'hA5);
    repeat (6) @(posedge PCLK);
    apb_read(`TCNT_ADDR, r);
    $display("After TDR write (load=0), TCNT = 0x%0h", r);

    // Create rising edge of TCR.load by writing load=1
    apb_write(`TCR_ADDR, 8'h00);
    @(posedge PCLK);
    apb_write(`TCR_ADDR, 8'h80); // load bit = 1
    repeat (6) @(posedge PCLK);
    apb_read(`TCNT_ADDR, r);
    $display("After TCR.load rising, TCNT = 0x%0h (expect previous TDR)", r);

    // Clear load and write TDR again
    apb_write(`TCR_ADDR, 8'h00);
    @(posedge PCLK);
    apb_write(`TDR_ADDR, 8'h5A);
    repeat (6) @(posedge PCLK);
    apb_read(`TCNT_ADDR, r);
    $display("After second TDR write, TCNT = 0x%0h", r);

    // -------------------------------------------------------------------------
    // NEW: Overflow (OVF) sequence
    // -------------------------------------------------------------------------
    do_reset();

    // 1) Load TDR with all 1's (max)
    apb_write(`TDR_ADDR, {DATA_WIDTH{1'b1}}); // uses localparam DATA_WIDTH (no backtick)
    @(posedge PCLK);

    // 2) Make TCR.load rising to load TDR -> TCNT
    apb_write(`TCR_ADDR, 8'h00);
    @(posedge PCLK);
    apb_write(`TCR_ADDR, 8'h80); // set load=1 => should load TDR into TCNT
    repeat (4) @(posedge PCLK);

    // 3) Now enable counting up: TCR En=1 (bit4), Up=0 (bit5=0), Cks=00
    apb_write(`TCR_ADDR, 8'b0001_0000); // 0x10
    repeat (2) @(posedge PCLK);
    apb_read(`TCNT_ADDR, r);
    $display("Before OVF pulse: TCNT = 0x%0h (should be 0x%0h)", r, {DATA_WIDTH{1'b1}});

    // 4) Force Clock_counter rising edges to increment and hit overflow branch
    $display("Forcing Clock_counter pulses to cause OVF...");
    for (k = 0; k < 2; k = k + 1) begin
      force dut.u_selclk.Clock_counter = 1'b1;
      @(posedge PCLK);
      force dut.u_selclk.Clock_counter = 1'b0;
      @(posedge PCLK);
    end
    release dut.u_selclk.Clock_counter;
    repeat (2) @(posedge PCLK);

    apb_read(`TCNT_ADDR, r);
    $display("After forced pulses (OVF expected): TCNT = 0x%0h, OVF_signal=%b", r, TMR_OVF);

    // -------------------------------------------------------------------------
    // NEW: Underflow (UDF) sequence
    // -------------------------------------------------------------------------
    do_reset();
    apb_write(`TDR_ADDR, 8'h00);
    @(posedge PCLK);
    apb_write(`TCR_ADDR, 8'h80); // load rising
    repeat (4) @(posedge PCLK);

    // set TCR to enable count down: Up/Dw=1 (bit5), En=1 (bit4) -> 0b0011_0000 = 0x30
    apb_write(`TCR_ADDR, 8'b0011_0000); // Up/Dw=1, En=1
    repeat (2) @(posedge PCLK);
    apb_read(`TCNT_ADDR, r);
    $display("Before UDF pulses: TCNT = 0x%0h (expect 0x00)", r);

    // Force pulses to decrement and cause underflow
    $display("Forcing Clock_counter pulses to cause UDF...");
    for (k = 0; k < 2; k = k + 1) begin
      force dut.u_selclk.Clock_counter = 1'b1;
      @(posedge PCLK);
      force dut.u_selclk.Clock_counter = 1'b0;
      @(posedge PCLK);
    end
    release dut.u_selclk.Clock_counter;
    repeat (2) @(posedge PCLK);

    apb_read(`TCNT_ADDR, r);
    $display("After forced pulses (UDF expected): TCNT = 0x%0h, UDF_signal=%b", r, TMR_URF);

    // -------------------------------------------------------------------------
    // Final touch: read all valid addresses to cover decode & read mux
    // -------------------------------------------------------------------------
    do_reset();
    apb_read(`TDR_ADDR,  r);  $display("TDR  = 0x%0h", r);
    apb_read(`TCR_ADDR,  r);  $display("TCR  = 0x%0h", r);
    apb_read(`TSR_ADDR,  r);  $display("TSR  = 0x%0h", r);
    apb_read(`TCNT_ADDR, r);  $display("TCNT = 0x%0h", r);

    if (err_cnt == 0)
      $display("TEST PASSED");
    else
      $display("TEST FAILED with %0d error(s)", err_cnt);

    #50 $finish;
  end
endmodule
