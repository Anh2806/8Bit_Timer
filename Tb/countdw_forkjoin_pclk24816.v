// countdw_forkjoin_pclk2.v — Final concise merged testbench for checklist 10→13 (Count DOWN)
// Fixed: apb_read_wait now has two outputs (data, pslverr) to match all calls.
// Verilog-2001 compatible.
`timescale 1ns/1ps
module countdw_forkjoin_pclk2;
  `include "Def_name.v"

  parameter ADDR_WIDTH = 8;
  parameter DATA_WIDTH = 8;

  // === APB master signals ===
  reg                       PCLK;
  reg                       PRESET_n;
  reg                       PSEL;
  reg                       PWRITE;
  reg                       PENABLE;
  reg  [ADDR_WIDTH-1:0]     PADDR;
  reg  [DATA_WIDTH-1:0]     PWDATA;
  wire [DATA_WIDTH-1:0]     PRDATA;
  wire                      PREADY;
  wire                      PSLVERR;

  // === DUT nets ===
  wire [DATA_WIDTH-1:0]     TDR_reg, TCR_reg, TSR_reg;
  wire                      TDR_WR_pulse;
  wire [DATA_WIDTH-1:0]     TCNT_Out;
  wire                      TMR_OVF, TMR_UDF;
  wire [1:0]                Cks;
  wire                      Load_Tdr;
  wire [DATA_WIDTH-1:0]     count_start_value;
  wire                      count_up_down;
  wire                      count_enable;
  wire                      Clock_counter;

  // === Instantiate DUT pieces ===
  Rw_register #(.ADDR_WIDTH(ADDR_WIDTH), .DATA_WIDTH(DATA_WIDTH)) u_rw (
    .PCLK(PCLK), .PRESET_n(PRESET_n),
    .PSEL(PSEL), .PWRITE(PWRITE), .PENABLE(PENABLE),
    .PADDR(PADDR), .PWDATA(PWDATA), .PRDATA(PRDATA),
    .PREADY(PREADY), .PSLVERR(PSLVERR),
    .TDR_reg(TDR_reg), .TCR_reg(TCR_reg), .TSR_reg(TSR_reg),
    .TDR_WR_pulse(TDR_WR_pulse),
    .TCNT_Out(TCNT_Out),
    .Set_OVF_pulse(TMR_OVF), .Set_UDF_pulse(TMR_UDF)
  );

  control_logic #(.DATA_WIDTH(DATA_WIDTH)) u_ctrl (
    .PCLK(PCLK), .PRESET_n(PRESET_n),
    .TDR_reg(TDR_reg), .TCR_reg(TCR_reg), .TDR_WR_pulse(TDR_WR_pulse),
    .Cks(Cks), .Load_Tdr(Load_Tdr), .count_start_value(count_start_value),
    .count_up_down(count_up_down), .count_enable(count_enable)
  );

  select_clock u_selclk (.PCLK(PCLK), .PRESET_n(PRESET_n), .Cks(Cks), .Clock_counter(Clock_counter));

  timer_counter #(.DATA_WIDTH(DATA_WIDTH)) u_cnt (
    .PCLK(PCLK), .PRESET_n(PRESET_n),
    .Clock_counter(Clock_counter),
    .count_start_value(count_start_value),
    .count_load(Load_Tdr),
    .count_enable(count_enable),
    .count_up_down(count_up_down),
    .TCNT_Out(TCNT_Out),
    .Set_OVF_pulse(TMR_OVF), .Set_UDF_pulse(TMR_UDF)
  );

  // === Clock / Reset ===
  initial PCLK = 0;
  always #5 PCLK = ~PCLK;

  task gen_reset;
    begin
      PRESET_n = 0;
      PSEL = 0; PWRITE = 0; PENABLE = 0;
      PADDR = {ADDR_WIDTH{1'b0}}; PWDATA = {DATA_WIDTH{1'b0}};
      repeat (5) @(posedge PCLK);
      PRESET_n = 1;
      repeat (2) @(posedge PCLK);
    end
  endtask

  // === APB helpers ===
  // Wait-style write (waits until PREADY asserted)
  task apb_write_wait;
    input [ADDR_WIDTH-1:0] addr;
    input [DATA_WIDTH-1:0] data;
    reg observed_pready0;
    begin
      @(posedge PCLK);
      PSEL <= 1; PWRITE <= 1; PADDR <= addr; PWDATA <= data; PENABLE <= 0;
      @(posedge PCLK);
      PENABLE <= 1;
      if (!PREADY) begin observed_pready0 = 1; $display("apb_write_wait: observed PREADY==0 at addr=0x%0h", addr); end
      while (!PREADY) @(posedge PCLK);
      @(posedge PCLK);
      PSEL <= 0; PWRITE <= 0; PENABLE <= 0; PADDR <= {ADDR_WIDTH{1'b0}}; PWDATA <= {DATA_WIDTH{1'b0}};
    end
  endtask

  // Nowait-style write: assert PSEL/PENABLE and sample PREADY on next cycle (used to hit immediate PREADY==0)
  task apb_write_nowait;
    input [ADDR_WIDTH-1:0] addr;
    input [DATA_WIDTH-1:0] data;
    reg sample_pready;
    begin
      @(posedge PCLK);
      PSEL <= 1; PWRITE <= 1; PADDR <= addr; PWDATA <= data; PENABLE <= 1;
      @(posedge PCLK);
      sample_pready = PREADY;
      if (!sample_pready) $display("apb_write_nowait: immediate PREADY==0 at addr=0x%0h", addr);
      // tidy up
      PSEL <= 0; PWRITE <= 0; PENABLE <= 0; PADDR <= {ADDR_WIDTH{1'b0}}; PWDATA <= {DATA_WIDTH{1'b0}};
      @(posedge PCLK);
    end
  endtask

  // Wait-style read (returns data and PSLVERR)
  task apb_read_wait;
    input [ADDR_WIDTH-1:0] addr;
    output [DATA_WIDTH-1:0] data;
    output                  pslverr;
    reg observed_pready0;
    begin
      @(posedge PCLK);
      PSEL <= 1; PWRITE <= 0; PADDR <= addr; PENABLE <= 0;
      @(posedge PCLK);
      PENABLE <= 1;
      if (!PREADY) begin observed_pready0 = 1; $display("apb_read_wait: observed PREADY==0 at addr=0x%0h", addr); end
      while (!PREADY) @(posedge PCLK);
      @(posedge PCLK);
      data = PRDATA;
      pslverr = PSLVERR;
      PSEL <= 0; PENABLE <= 0; PADDR <= {ADDR_WIDTH{1'b0}};
    end
  endtask

  // One-cycle read (combinational/quick read path)
  task apb_read_once;
    input [ADDR_WIDTH-1:0] addr;
    output [DATA_WIDTH-1:0] data;
    begin
      @(posedge PCLK);
      PSEL <= 1; PWRITE <= 0; PADDR <= addr; PENABLE <= 1;
      @(posedge PCLK);
      data = PRDATA;
      PSEL <= 0; PENABLE <= 0; PADDR <= {ADDR_WIDTH{1'b0}};
      @(posedge PCLK);
    end
  endtask

  // === small helpers ===
  function integer get_udf; input [DATA_WIDTH-1:0] tsr; begin get_udf = tsr[`TMR_UDF_bit]; end endfunction
  function integer get_ovf; input [DATA_WIDTH-1:0] tsr; begin get_ovf = tsr[`TMR_OVF_bit]; end endfunction

  task clear_udf; reg [DATA_WIDTH-1:0] tmp; reg pslv; begin tmp = (1<<`TMR_UDF_bit); apb_write_wait(`TSR_ADDR, tmp); apb_read_wait(`TSR_ADDR, tmp, pslv); end endtask
  task clear_ovf; reg [DATA_WIDTH-1:0] tmp; reg pslv; begin tmp = (1<<`TMR_OVF_bit); apb_write_wait(`TSR_ADDR, tmp); apb_read_wait(`TSR_ADDR, tmp, pslv); end endtask

  // Map CKS -> factor (pclk multiply)
  function integer factor_of_cks; input [1:0] cks; begin case (cks) 2'b00: factor_of_cks=2; 2'b01: factor_of_cks=4; 2'b10: factor_of_cks=8; default: factor_of_cks=16; endcase end endfunction

  // Build TCR value, try set LOAD bit if macro exists
  function [7:0] make_tcr;
    input [1:0] cks_bits;
    input en;
    input down;
    input load_bit;
    reg [7:0] t;
    begin
      t = 8'h00;
      t[`TCR_Cks_1:`TCR_Cks_0] = cks_bits;
      t[`TCR_En_bit] = en;
      t[`TCR_Up_Dw_bit] = down;
      `ifdef TCR_Load_bit
        t[`TCR_Load_bit] = load_bit;
      `endif
      make_tcr = t;
    end
  endfunction

  integer err_cnt;

  // === core fork-join countdown test ===
  task run_one;
    input [1:0] cks_bits;
    integer start, factor, steps, pulses_needed;
    reg [DATA_WIDTH-1:0] tsr1, tsr2;
    reg err;
    reg [7:0] tcr;
    begin
      // pseudo random start in [8..200]
      start = $random; if (start<0) start = -start; start = (start % 193) + 8;
      apb_write_wait(`TDR_ADDR, start[7:0]);
      tcr = make_tcr(cks_bits, 1'b1, 1'b1, 1'b0); // EN=1, DOWN=1, LOAD=0
      apb_write_wait(`TCR_ADDR, tcr);

      factor = factor_of_cks(cks_bits);
      steps = start + 1;
      pulses_needed = steps * factor;

      $display("\n--- COUNTDOWN: CKS=%02b factor=%0d start=%0d pulses=%0d ---", cks_bits, factor, start, pulses_needed);

      fork
        begin : THREAD1
          repeat (pulses_needed + 2) @(posedge PCLK);
          apb_read_wait(`TSR_ADDR, tsr1, err);
          if (get_udf(tsr1) && !get_ovf(tsr1)) $display("T1 PASS TSR=0x%0h", tsr1);
          else begin $display("T1 FAIL TSR=0x%0h", tsr1); err_cnt = err_cnt + 1; end
        end
        begin : THREAD2
          integer short_cycles;
          short_cycles = (pulses_needed * 2) / 3;
          repeat (short_cycles) @(posedge PCLK);
          apb_read_wait(`TSR_ADDR, tsr2, err);
          if (!get_udf(tsr2) && !get_ovf(tsr2)) $display("T2 PASS TSR=0x%0h", tsr2);
          else begin $display("T2 FAIL TSR=0x%0h", tsr2); err_cnt = err_cnt + 1; end
        end
      join

      clear_udf(); clear_ovf();
    end
  endtask

  // invalid write to hit default/PSLVERR in Rw_register
  task hit_invalid_write;
    reg [DATA_WIDTH-1:0] rdata; reg rerr;
    begin
      $display("\n--- invalid write (default path) ---");
      apb_write_nowait(8'hFF, 8'hAA);
      apb_read_wait(`TSR_ADDR, rdata, rerr);
      $display("After invalid write: PRDATA=0x%0h PSLVERR=%0d", rdata, rerr);
    end
  endtask

  // hit TCR LOAD=1 branch and read TCNT path
  task hit_tcr_load_and_read_tcnt;
    reg [DATA_WIDTH-1:0] rdata; reg rerr;
    begin
      $display("\n--- hit TCR LOAD=1 branch + read TCNT ---");
      apb_write_wait(`TDR_ADDR, 8'h3A);
      // Set LOAD=1, EN=0 (so timer doesn't start); if TCR_Load_bit macro absent, LOAD ignored
      apb_write_wait(`TCR_ADDR, make_tcr(2'b00, 1'b0, 1'b1, 1'b1));
      // read TCNT using quick read (combinational read path)
      apb_read_once(`TCNT_ADDR, rdata);
      $display("TCNT read (once) PRDATA=0x%0h", rdata);
      // clear LOAD back to 0
      apb_write_wait(`TCR_ADDR, make_tcr(2'b00, 1'b0, 1'b1, 1'b0));
    end
  endtask
  reg [DATA_WIDTH-1:0] tmpd; reg tmp_err;
  // === main ===
  initial begin
    err_cnt = 0;
    gen_reset();

    // 1) write TDR to generate TDR_WR_pulse
    apb_write_wait(`TDR_ADDR, 8'h5A);
    @(posedge PCLK);

    // 2) write TCR with LOAD = 1 (if macro exists in Def_name.v)
    apb_write_wait(`TCR_ADDR, make_tcr(2'b00, 1'b0, 1'b1, 1'b1)); // EN=0, LOAD=1
    // wait a couple of edges so always @(posedge PCLK ...) updates load_bit_d
    repeat (2) @(posedge PCLK);

    // 3) clear LOAD
    apb_write_wait(`TCR_ADDR, make_tcr(2'b00, 1'b0, 1'b1, 1'b0));
    repeat (2) @(posedge PCLK);

    // 4) read back TCNT/TDR/TCR/TSR to cover read paths
    apb_read_once(`TCNT_ADDR, tmpd);
    apb_read_wait(`TDR_ADDR, tmpd, tmp_err);
    apb_read_wait(`TCR_ADDR, tmpd, tmp_err);
    apb_read_wait(`TSR_ADDR, tmpd, tmp_err);
    // Checklist 10..13
    run_one(2'b00); // pclk×2
    run_one(2'b01); // pclk×4
    run_one(2'b10); // pclk×8
    run_one(2'b11); // pclk×16

    // Additional branches
    hit_invalid_write();
    hit_tcr_load_and_read_tcnt();

    if (err_cnt == 0) $display("\n[SUMMARY] Tests passed (no failures counted).");
    else $display("\n[SUMMARY] Tests finished with err_cnt=%0d", err_cnt);
    #200 $finish;
  end

endmodule
