// count_pause_both_pclk2.v
// Gộp checklist: countup_pause_countup_pclk2 + countdw_pause_countdw_pclk2
// Giả định các macro trong Def_name.v:
//  TDR_ADDR,TCR_ADDR,TSR_ADDR,TCNT_ADDR,
//  TCR_En_bit, TCR_Up_Dw_bit, TCR_Cks_1:TCR_Cks_0,
//  TMR_UDF_bit, TMR_OVF_bit
`timescale 1ns/1ps
module upup_pause_dwdw_pclk2;
  `include "Def_name.v"

  parameter ADDR_WIDTH = 8;
  parameter DATA_WIDTH = 8;

  // APB master signals
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

  // DUT nets (hookup)
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

  // ---------------- DUT ----------------
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

  // ---------------- Clock / Reset ----------------
  initial PCLK = 0;
  always #5 PCLK = ~PCLK; // 10 ns

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

  // ---------------- APB helpers ----------------
  task apb_write_wait;
    input [ADDR_WIDTH-1:0] addr;
    input [DATA_WIDTH-1:0] data;
    begin
      @(posedge PCLK);
      // use blocking assigns so signals change immediately in this procedural flow
      PSEL   = 1;
      PWRITE = 1;
      PADDR  = addr;
      PWDATA = data;
      PENABLE = 0;
      @(posedge PCLK);
      PENABLE = 1;
      // wait for slave ready (handles wait-state model)
      while (!PREADY) @(posedge PCLK);
      @(posedge PCLK);
      // complete transaction: deassert signals and clear addr/data
      PSEL   = 0;
      PWRITE = 0;
      PENABLE = 0;
      PADDR  = {ADDR_WIDTH{1'b0}};
      PWDATA = {DATA_WIDTH{1'b0}};
    end
  endtask

  // Nowait write: sample PREADY immediately on first response (for coverage)
  // Useful to hit the case where slave responds with PREADY==0 for one cycle.
  task apb_write_nowait_force_pready0;
    input [ADDR_WIDTH-1:0] addr;
    input [DATA_WIDTH-1:0] data;
    reg sample_pready;
    begin
      // Prepare bus signals so they will be sampled on next rising PCLK
      @(posedge PCLK);
      // Use blocking assigns here to take effect immediately in TB flow
      PSEL   = 1;
      PWRITE = 1;
      PADDR  = addr;
      PWDATA = data;
      PENABLE = 1;

      // force the slave's PREADY low BEFORE the next posedge so the slave sees PREADY==0
      // IMPORTANT: ensure the instance name matches YOUR DUT instance (here it's u_rw)
      force u_rw.PREADY = 1'b0;

      // Wait the sampling edge (Rw_register samples at posedge PCLK)
      @(posedge PCLK);

      // sample what the DUT/monitor wire sees
      sample_pready = PREADY;
      $display("%0t: apb_write_nowait_force_pready0: sampled PREADY=%b (addr=0x%0h)",
                $time, sample_pready, addr);

      // Keep forced low one extra cycle if you want the apb master to see wait-state
      // then release and tear down
      @(posedge PCLK);
      release u_rw.PREADY;

      // teardown bus signals
      PSEL   = 0;
      PWRITE = 0;
      PENABLE = 0;
      PADDR  = {ADDR_WIDTH{1'b0}};
      PWDATA = {DATA_WIDTH{1'b0}};
      @(posedge PCLK); // allow design to settle
    end
endtask

  // Read with wait: standard APB read (assert PSEL, then PENABLE, wait PREADY, sample PRDATA)
  task apb_read_wait;
    input [ADDR_WIDTH-1:0] addr;
    output [DATA_WIDTH-1:0] data;
    output                   pslverr;
    begin
      @(posedge PCLK);
      PSEL   = 1;
      PWRITE = 0;
      PADDR  = addr;
      PENABLE = 0;
      @(posedge PCLK);
      PENABLE = 1;
      while (!PREADY) @(posedge PCLK);
      @(posedge PCLK);
      // sample read data & PSLVERR
      data = PRDATA;
      pslverr = PSLVERR;
      // teardown bus signals
      PSEL   = 0;
      PENABLE = 0;
      PADDR  = {ADDR_WIDTH{1'b0}};
      @(posedge PCLK);
    end
  endtask

  // Quick one-cycle read (uses combinational PRDATA path if DUT supports it)
  // Useful to exercise the one-cycle read case and coverage.
  task apb_read_once;
    input [ADDR_WIDTH-1:0] addr;
    output [DATA_WIDTH-1:0] data;
    begin
      @(posedge PCLK);
      PSEL   = 1;
      PWRITE = 0;
      PADDR  = addr;
      PENABLE = 1; // combine select+enable for single-cycle read
      @(posedge PCLK);
      data = PRDATA;
      // teardown
      PSEL = 0;
      PENABLE = 0;
      PADDR  = {ADDR_WIDTH{1'b0}};
      @(posedge PCLK);
    end
  endtask
  
  // make_tcr: luôn set đúng LOAD dù macro chữ thường hay chữ hoa
  function [7:0] make_tcr;
    input [1:0] cks_bits; input en; input down; input load_bit;
    reg [7:0] t;
    begin
      t = 8'h00;
      t[`TCR_Cks_1:`TCR_Cks_0] = cks_bits;
      t[`TCR_En_bit]    = en;
      t[`TCR_Up_Dw_bit] = down;
`ifdef TCR_load_bit
      t[`TCR_load_bit]  = load_bit;
`elsif TCR_Load_bit
      t[`TCR_Load_bit]  = load_bit;
`endif
      make_tcr = t;
    end
  endfunction

  // dọn TSR (clear 2 bit) – tiện cho từng case
  task clear_tsr_all;
    reg [DATA_WIDTH-1:0] tmp; reg e;
    begin
      tmp = (1<<`TMR_OVF_bit) | (1<<`TMR_UDF_bit);
      apb_write_wait(`TSR_ADDR, tmp);
      repeat (2) @(posedge PCLK);
      apb_read_wait(`TSR_ADDR, tmp, e);
      $display("  [clear_tsr_all] TSR=0x%0h", tmp);
    end
  endtask

  // tạo cạnh LOAD 0->1 để hit nhánh load_q & ~load_qq trong control_logic
  task cover_load_edge_from_tcr_only;
    reg [DATA_WIDTH-1:0] rd; reg err;
    begin
      $display("\n--- cover_load_edge_from_tcr_only ---");
      apb_write_wait(`TCR_ADDR, make_tcr(2'b00, 1'b0, 1'b0, 1'b0)); // LOAD=0
      repeat (2) @(posedge PCLK);
      apb_write_wait(`TCR_ADDR, make_tcr(2'b00, 1'b0, 1'b0, 1'b1)); // LOAD=1 (tạo cạnh)
      @(posedge PCLK);
      repeat (2) @(posedge PCLK); // giữ chút để cover các pattern
      apb_write_wait(`TCR_ADDR, make_tcr(2'b00, 1'b0, 1'b0, 1'b0)); // về 0
      repeat (2) @(posedge PCLK);
      apb_read_wait(`TCNT_ADDR, rd, err);
      $display("--- done, TCNT=0x%0h", rd);
    end
  endtask

  // gọi apb_read_once để có coverage
  task exercise_read_once_for_coverage;
    reg [DATA_WIDTH-1:0] d;
    begin
      $display("\n--- exercise_read_once_for_coverage ---");
      apb_read_once(`TCNT_ADDR, d);
      $display("  one-cycle read TCNT=0x%0h", d);
    end
  endtask

  // quay vòng CKS để tăng coverage select_clock
  task cycle_cks_for_coverage;
    integer i; reg [DATA_WIDTH-1:0] d; reg e;
    begin
      $display("\n--- cycle_cks_for_coverage (00..11) ---");
      for (i=0; i<4; i=i+1) begin
        apb_write_wait(`TCR_ADDR, make_tcr(i[1:0], 1'b0, 1'b0, 1'b0));
        repeat (6) @(posedge PCLK);
        apb_read_wait(`TCNT_ADDR, d, e);
        $display("  CKS=%02b -> TCNT=0x%0h", i[1:0], d);
      end
    end
  endtask

  // ---------------- Tham số thời lượng ----------------
  parameter PAUSE_CYCLES    = 50; // số chu kỳ PCLK dừng (pause)
  parameter INTERNAL_FACTOR = 2;  // pclk×2 => mỗi tick bộ đếm ~ 2 PCLK

  integer err_cnt;

  // -------- Subtest: pause rồi tiếp tục count-up --------
   function integer get_udf;
    input [DATA_WIDTH-1:0] tsr;
    begin
      get_udf = tsr[`TMR_UDF_bit];
    end
  endfunction

  function integer get_ovf;
    input [DATA_WIDTH-1:0] tsr;
    begin
      get_ovf = tsr[`TMR_OVF_bit];
    end
  endfunction
  
  task run_pause_countup;
    input integer start_val; // 0..255
    reg [DATA_WIDTH-1:0] tsr; reg pslv;
    integer run_cycles;
    begin
      $display("\n=== SUBTEST: pause-countup (pclk x2), start=%0d ===", start_val);

      clear_tsr_all();
      apb_write_wait(`TDR_ADDR, start_val[7:0]);
      apb_write_wait(`TCR_ADDR, make_tcr(2'b00, 1'b1, 1'b0, 1'b0)); // EN=1, UP

      run_cycles = (start_val < 200) ? (50*INTERNAL_FACTOR) : (10*INTERNAL_FACTOR);
      repeat (run_cycles) @(posedge PCLK);

      // pause
      apb_write_wait(`TCR_ADDR, make_tcr(2'b00, 1'b0, 1'b0, 1'b0));
      $display("-> paused for %0d cycles", PAUSE_CYCLES);
      repeat (PAUSE_CYCLES) @(posedge PCLK);

      // trạng thái trong pause
      apb_read_wait(`TSR_ADDR, tsr, pslv);
      if (get_ovf(tsr)) $display("During pause: OVF=1 -> FAULTY");
      else               $display("During pause: OVF=0 -> PASS");

      // resume
      apb_write_wait(`TCR_ADDR, make_tcr(2'b00, 1'b1, 1'b0, 1'b0));
      // đợi đến overflow
      apb_read_wait(`TCNT_ADDR, tsr, pslv);
      run_cycles = (256 - tsr) * INTERNAL_FACTOR + 4;
      repeat (run_cycles) @(posedge PCLK);

      apb_read_wait(`TSR_ADDR, tsr, pslv);
      if (get_ovf(tsr)) $display("After resume: OVF=1 -> PASS");
      else begin $display("After resume: OVF=0 -> FAULTY"); err_cnt = err_cnt + 1; end

      apb_write_wait(`TSR_ADDR, (1<<`TMR_OVF_bit)); // clear OVF
      apb_read_wait(`TSR_ADDR, tsr, pslv);
    end
  endtask

  // -------- Subtest: pause rồi tiếp tục count-down --------
  task run_pause_countdown;
    input integer start_val; // 0..255
    reg [DATA_WIDTH-1:0] tsr; reg pslv;
    integer run_cycles;
    begin
      $display("\n=== SUBTEST: pause-countdown (pclk x2), start=%0d ===", start_val);

      clear_tsr_all();
      apb_write_wait(`TDR_ADDR, start_val[7:0]);
      apb_write_wait(`TCR_ADDR, make_tcr(2'b00, 1'b1, 1'b1, 1'b0)); // EN=1, DOWN

      run_cycles = (start_val > 50) ? (50*INTERNAL_FACTOR) : (10*INTERNAL_FACTOR);
      repeat (run_cycles) @(posedge PCLK);

      // pause
      apb_write_wait(`TCR_ADDR, make_tcr(2'b00, 1'b0, 1'b1, 1'b0));
      $display("-> paused for %0d cycles", PAUSE_CYCLES);
      repeat (PAUSE_CYCLES) @(posedge PCLK);

      // trạng thái trong pause
      apb_read_wait(`TSR_ADDR, tsr, pslv);
      if (get_udf(tsr)) $display("During pause: UDF=1 -> FAULTY");
      else               $display("During pause: UDF=0 -> PASS");

      // resume
      apb_write_wait(`TCR_ADDR, make_tcr(2'b00, 1'b1, 1'b1, 1'b0));
      // đợi đến underflow
      apb_read_wait(`TCNT_ADDR, tsr, pslv);
      run_cycles = (tsr + 1) * INTERNAL_FACTOR + 4;
      repeat (run_cycles) @(posedge PCLK);

      apb_read_wait(`TSR_ADDR, tsr, pslv);
      if (get_udf(tsr)) $display("After resume: UDF=1 -> PASS");
      else begin $display("After resume: UDF=0 -> FAULTY"); err_cnt = err_cnt + 1; end

      apb_write_wait(`TSR_ADDR, (1<<`TMR_UDF_bit)); // clear UDF
      apb_read_wait(`TSR_ADDR, tsr, pslv);
    end
  endtask

  // ---------------- main ----------------
  integer s1, s2;
  initial begin
    err_cnt = 0;
    gen_reset(); 
    
    $display(">>> START debug: try apb_write_nowait_force_pready0");
    apb_write_wait(`TDR_ADDR, 8'hAA);           // safe normal write to put reg in known state
    apb_write_nowait_force_pready0(`TDR_ADDR, 8'h55); // try to hit PREADY==0
    repeat (3) @(posedge PCLK);

    // chọn start value an toàn khỏi wrap tức thì
    s1 = $random; if (s1<0) s1 = -s1; s1 = (s1 % 200) + 8; // count-up
    s2 = $random; if (s2<0) s2 = -s2; s2 = (s2 % 200) + 8; // count-down

    // hai subtest pause/resume
    run_pause_countup(s1);
    repeat (10) @(posedge PCLK);
    run_pause_countdown(s2);

    // các bài phụ để nâng coverage
    exercise_read_once_for_coverage();  // hit task apb_read_once
    cover_load_edge_from_tcr_only();    // hit nhánh load_q & ~load_qq
    cycle_cks_for_coverage();           // cover select_clock

    // summary
    if (err_cnt == 0) $display("\n[SUMMARY] Both pause tests PASS (no failures counted).");
    else              $display("\n[SUMMARY] Pause tests finished with err_cnt=%0d", err_cnt);

    #200 $finish;
  end

endmodule
