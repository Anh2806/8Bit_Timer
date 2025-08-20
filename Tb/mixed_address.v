// mixed_address.v - Verilog-2001 (updated to exercise branches & coverage)
`timescale 1ns/1ps
module mixed_address;
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

  // DUT instance (adjust names inside timer_top if different)
  timer_top #(.ADDR_WIDTH(ADDR_WIDTH), .DATA_WIDTH(DATA_WIDTH)) dut (
    .PCLK(PCLK), .PRESET_n(PRESET_n),
    .PSEL(PSEL), .PWRITE(PWRITE), .PENABLE(PENABLE),
    .PADDR(PADDR), .PWDATA(PWDATA),
    .PRDATA(PRDATA), .PREADY(PREADY), .PSLVERR(PSLVERR),
    .TMR_OVF(TMR_OVF), .TMR_URF(TMR_URF)
  );

  // clock
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

  // APB write (simple transfer)
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

  // APB read (simple transfer)
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

  // forced-wait helpers (cover PREADY==0 on both read & write)
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
      // Hierarchical force - adjust path if your instance name differs
      force dut.u_apb.PREADY = 1'b0;
      for (j = 0; j < wait_cycles; j = j + 1) @(posedge PCLK);
      release dut.u_apb.PREADY;
      while (!PREADY) @(posedge PCLK);
      @(posedge PCLK);
      PSEL <= 1'b0; PWRITE <= 1'b0; PENABLE <= 1'b0;
    end
  endtask

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
  reg [DATA_WIDTH-1:0] r, w, tmp;
  reg [ADDR_WIDTH-1:0] a;
  integer k;

  initial begin
    err_cnt = 0;
    do_reset();

    // random mixed writes and checks (cover both valid/invalid addresses)
    for (i = 0; i < LOOPS; i = i + 1) begin
      a = rand8(0); w = rand8(0);
      apb_write(a, w);

      // invalid address -> PSLVERR expected
      if ((a != `TDR_ADDR) && (a != `TCR_ADDR) && (a != `TSR_ADDR) && (a != `TCNT_ADDR)) begin
        if (!PSLVERR) begin
          $display("[%0t] ERROR: invalid write addr=0x%0h no PSLVERR", $time, a);
          err_cnt = err_cnt + 1;
        end else begin
          $display("[%0t] NULL-ADDRESS write addr=0x%0h PSLVERR=1", $time, a);
        end
      end else begin
        if (PSLVERR) begin
          $display("[%0t] ERROR: PSLVERR asserted for valid addr=0x%0h", $time, a);
          err_cnt = err_cnt + 1;
        end
        // read back and check by register type
        apb_read(a, r);
        if (a == `TDR_ADDR) begin
          if (r !== w) begin
            $display("[%0t] ERROR: TDR wrote=0x%0h read=0x%0h", $time, w, r);
            err_cnt = err_cnt + 1;
          end
        end else if (a == `TCR_ADDR) begin
          if ((r & TCR_MASK) !== (w & TCR_MASK)) begin
            $display("[%0t] ERROR: TCR masked mismatch wrote=0x%0h read=0x%0h mask=0x%0h", $time, w, r, TCR_MASK);
            err_cnt = err_cnt + 1;
          end
        end else if (a == `TSR_ADDR) begin
          if (r !== {DATA_WIDTH{1'b0}}) begin
            $display("[%0t] ERROR: TSR expected 0 read=0x%0h", $time, r);
            err_cnt = err_cnt + 1;
          end
        end else if (a == `TCNT_ADDR) begin
          $display("[%0t] read TCNT = 0x%0h", $time, r);
        end
      end

      // occasionally hit forced-wait branches to exercise PREADY_0
      if (i == 4) begin
        apb_read_with_forced_wait(`TCNT_ADDR, tmp, 3);
        $display("[%0t] forced-read TCNT=%0h", $time, tmp);
      end
      if (i == 9) begin
        apb_write_with_forced_wait(`TCR_ADDR, 8'h5A, 3);
        apb_read(`TCR_ADDR, tmp); $display("[%0t] after forced-write TCR=%0h", $time, tmp);
      end
    end

    // -----------------------
    // Extra sequences to hit control_logic/timer_counter branches
    // -----------------------
    // 1) Simple TDR write + ensure load via TCR.load rising edge
    apb_write(`TDR_ADDR, 8'h3C);
    @(posedge PCLK);
    // create TCR.load rising edge
    apb_write(`TCR_ADDR, 8'h00); @(posedge PCLK);
    apb_write(`TCR_ADDR, 8'h80); @(posedge PCLK); // load=1
    apb_write(`TCR_ADDR, 8'h00); @(posedge PCLK); // load cleared
    repeat (2) @(posedge PCLK);
    apb_read(`TCNT_ADDR, tmp); $display("[%0t] After load, TCNT=0x%0h", $time, tmp);

    // 2) change Cks to exercise select_clock selection
    apb_write(`TCR_ADDR, 8'b0000_0000); @(posedge PCLK);
    apb_write(`TCR_ADDR, 8'b0000_0001); @(posedge PCLK);
    apb_write(`TCR_ADDR, 8'b0000_0010); @(posedge PCLK);
    apb_write(`TCR_ADDR, 8'b0000_0011); @(posedge PCLK);

    // 3) Attempt to cause overflow (OVF) by loading all 1s then forcing two clean rising edges
    // set baseline Clock_counter = 0
    force dut.u_selclk.Clock_counter = 1'b0;
    repeat (2) @(posedge PCLK);

    // load TDR = all ones and make it effective
    apb_write(`TDR_ADDR, {DATA_WIDTH{1'b1}});
    @(posedge PCLK);
    apb_write(`TCR_ADDR, 8'h80); @(posedge PCLK); // load=1 - rising
    apb_write(`TCR_ADDR, 8'h00); @(posedge PCLK); // clear
    repeat (2) @(posedge PCLK);

    // enable counting up (bit4)
    apb_write(`TCR_ADDR, 8'h10); @(posedge PCLK); @(posedge PCLK);

    // create two clean rising edges that timer_counter can sample
    $display("[%0t] Forcing two clean Clock_counter rising edges (OVF attempt)", $time);
    force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);
    force dut.u_selclk.Clock_counter = 1'b1; @(posedge PCLK); // sampled rising edge
    force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);

    force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);
    force dut.u_selclk.Clock_counter = 1'b1; @(posedge PCLK); // sampled rising edge
    force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);

    release dut.u_selclk.Clock_counter;
    repeat (2) @(posedge PCLK);
    apb_read(`TCNT_ADDR, tmp);
    $display("[%0t] After forced OVF pulses: TCNT=0x%0h OVF=%b", $time, tmp, TMR_OVF);

    // 4) Try underflow similarly: load 0, enable down, force pulses
    force dut.u_selclk.Clock_counter = 1'b0; repeat (2) @(posedge PCLK);
    apb_write(`TDR_ADDR, 8'h00); @(posedge PCLK);
    apb_write(`TCR_ADDR, 8'h80); @(posedge PCLK); apb_write(`TCR_ADDR, 8'h00); @(posedge PCLK);
    apb_write(`TCR_ADDR, 8'h30); @(posedge PCLK); // Up/Dw=1, En=1
    repeat (1) @(posedge PCLK);

    $display("[%0t] Forcing pulses for UDF attempt", $time);
    force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);
    force dut.u_selclk.Clock_counter = 1'b1; @(posedge PCLK); force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);
    force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);
    force dut.u_selclk.Clock_counter = 1'b1; @(posedge PCLK); force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);
    release dut.u_selclk.Clock_counter;
    repeat (2) @(posedge PCLK);
    apb_read(`TCNT_ADDR, tmp);
    $display("[%0t] After forced UDF pulses: TCNT=0x%0h UDF=%b", $time, tmp, TMR_URF);

    // 5) final reads to hit PRDATA multiplexing
    apb_read(`TDR_ADDR, r); $display("[%0t] Final TDR = 0x%0h", $time, r);
    apb_read(`TCR_ADDR, r); $display("[%0t] Final TCR = 0x%0h", $time, r);
    apb_read(`TSR_ADDR, r); $display("[%0t] Final TSR = 0x%0h", $time, r);
    apb_read(`TCNT_ADDR, r); $display("[%0t] Final TCNT = 0x%0h", $time, r);

    if (err_cnt == 0) $display("mixed_address PASSED");
    else $display("mixed_address FAILED with %0d error(s)", err_cnt);

    #100 $finish;
  end

endmodule
