`timescale 1ns/1ps
`include "Def_name.v"

module fake_over_under_flow;
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

  // DUT nets
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

  // Instantiate DUT
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
    .PCLK          (PCLK),
    .PRESET_n      (PRESET_n),
    .TDR_reg       (TDR_reg),
    .TCR_reg       (TCR_reg),
    .TDR_WR_pulse  (TDR_WR_pulse),
    .Cks           (Cks),
    .Load_Tdr      (Load_Tdr),
    .count_start_value (count_start_value),
    .count_up_down (count_up_down),
    .count_enable  (count_enable)
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

  // Clock / Reset
  initial PCLK = 0;
  always #5 PCLK = ~PCLK; // 10ns period

  // Monitor pulses from timer (help debug)
  always @(posedge PCLK) begin
    if (TMR_OVF) $display("%0t: Set_OVF_pulse asserted", $time);
    if (TMR_UDF) $display("%0t: Set_UDF_pulse asserted", $time);
  end

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
      PSEL <= 1; PWRITE <= 1; PADDR <= addr; PWDATA <= data; PENABLE <= 0;
      @(posedge PCLK);
      PENABLE <= 1;
      while (!PREADY) @(posedge PCLK);
      @(posedge PCLK);
      PSEL <= 0; PWRITE <= 0; PENABLE <= 0; PADDR <= {ADDR_WIDTH{1'b0}}; PWDATA <= {DATA_WIDTH{1'b0}};
    end
  endtask

  // dùng cho coverage điều kiện (PREADY==0) nếu RTL có wait-state
  task apb_write_nowait;
    input [ADDR_WIDTH-1:0] addr;
    input [DATA_WIDTH-1:0] data;
    reg sample_pready;
    begin
      @(posedge PCLK);
      PSEL <= 1; PWRITE <= 1; PADDR <= addr; PWDATA <= data; PENABLE <= 1;
      @(posedge PCLK);
      sample_pready = PREADY;
      if (!sample_pready) $display("apb_write_nowait: PREADY==0 at addr=0x%0h (coverage hit)", addr);
      PSEL <= 0; PWRITE <= 0; PENABLE <= 0;
      @(posedge PCLK);
    end
  endtask

  task apb_read_wait;
    input [ADDR_WIDTH-1:0] addr;
    output [DATA_WIDTH-1:0] data;
    output                   pslverr;
    begin
      @(posedge PCLK);
      PSEL <= 1; PWRITE <= 0; PADDR <= addr; PENABLE <= 0;
      @(posedge PCLK);
      PENABLE <= 1;
      while (!PREADY) @(posedge PCLK);
      @(posedge PCLK);
      data = PRDATA;
      pslverr = PSLVERR;
      PSEL <= 0; PENABLE <= 0; PADDR <= {ADDR_WIDTH{1'b0}};
    end
  endtask

  // Compose TCR byte (LOAD bit dùng macro lowercase sẵn có)
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
      t[`TCR_load_bit] = load_bit;
      make_tcr = t;
    end
  endfunction

  // Helper: toggle LOAD để TCNT := TDR (EN có thể 0 hoặc 1)
  task load_from_tdr;
    input [7:0] val;
    input dir_down; // 1: down, 0: up
    input en_bit;   // 0: disabled, 1: enabled
    begin
      apb_write_wait(`TDR_ADDR, val);
      apb_write_wait(`TCR_ADDR, make_tcr(2'b00, en_bit, dir_down, 1'b1)); // LOAD=1
      @(posedge PCLK);
      apb_write_wait(`TCR_ADDR, make_tcr(2'b00, en_bit, dir_down, 1'b0)); // LOAD=0
      @(posedge PCLK);
    end
  endtask

  // ---------------- Tests ----------------
  reg [DATA_WIDTH-1:0] data_r; reg err;

  // helper: check TSR flags against expected values
  task check_flags;
    input expect_ovf;
    input expect_udf;
    reg [DATA_WIDTH-1:0] tsr;
    reg pslv;
    begin
      apb_read_wait(`TSR_ADDR, tsr, pslv);
      $display("  TSR read = 0x%0h  OVF=%b  UDF=%b  PSLVERR=%b", tsr, tsr[`TMR_OVF_bit], tsr[`TMR_UDF_bit], pslv);
      if (tsr[`TMR_OVF_bit] !== expect_ovf) $display("  >>> OVF MISMATCH: expected %0d", expect_ovf);
      if (tsr[`TMR_UDF_bit] !== expect_udf) $display("  >>> UDF MISMATCH: expected %0d", expect_udf);
      if ((tsr[`TMR_OVF_bit] === expect_ovf) && (tsr[`TMR_UDF_bit] === expect_udf))
        $display("  Flags OK (matches expected)");
    end
  endtask

  // 1) Fake UNDERFLOW: EN=0, down, load 0 rồi 255 -> UDF không được set
  task run_fake_underflow;
    begin
      $display("\n[fake_underflow] start");
      $display("  Macro mapping: TMR_OVF_bit=%0d  TMR_UDF_bit=%0d", `TMR_OVF_bit, `TMR_UDF_bit);
      apb_write_wait(`TCR_ADDR, make_tcr(2'b00, 1'b0, 1'b1, 1'b0)); // EN=0, down
      load_from_tdr(8'h00, 1'b1, 1'b0);
      load_from_tdr(8'hFF, 1'b1, 1'b0);
      apb_read_wait(`TSR_ADDR, data_r, err);
      if (data_r[`TMR_UDF_bit]) $display("[fake_underflow] **UNEXPECTED** UDF=1");
      else                      $display("[fake_underflow] PASS: UDF=0");
    end
  endtask

  // 2) Fake OVERFLOW: EN=0, up, load 255 rồi 0 -> OVF không được set
  task run_fake_overflow;
    begin
      $display("\n[fake_overflow] start");
      apb_write_wait(`TCR_ADDR, make_tcr(2'b00, 1'b0, 1'b0, 1'b0)); // EN=0, up
      load_from_tdr(8'hFF, 1'b0, 1'b0);
      load_from_tdr(8'h00, 1'b0, 1'b0);
      apb_read_wait(`TSR_ADDR, data_r, err);
      if (data_r[`TMR_OVF_bit]) $display("[fake_overflow] **UNEXPECTED** OVF=1");
      else                      $display("[fake_overflow] PASS: OVF=0");
    end
  endtask

  // 3) Thực sự đếm để cover nhánh count_up / count_down và wrap
  task run_real_count_branches;
    integer i;
    begin
      $display("\n[real_count] exercise branches up/non-wrap, up/wrap, down/non-wrap, down/wrap");

      // (a) Count UP non-wrap (start=0x10 -> +4)
      load_from_tdr(8'h10, 1'b0, 1'b1);          // EN=1, up
      repeat (10) @(posedge PCLK);               // chờ đồng hồ nội
      apb_read_wait(`TCNT_ADDR, data_r, err);
      $display("[real_count] up/non-wrap TCNT=0x%0h", data_r);

      // (b) Count UP wrap (start=0xFF -> wrap về 0, OVF pulse)
      load_from_tdr(8'hFF, 1'b0, 1'b1);          // EN=1, up
      repeat (20) @(posedge PCLK);               // tăng chờ để chắc wrap
      $display("[real_count] up/wrap - check flags (expect OVF=1)");
      check_flags(1,0);                          // mong OVF=1, UDF=0
      apb_write_wait(`TSR_ADDR, (1<<`TMR_OVF_bit)); // clear OVF

      // (c) Count DOWN non-wrap (start=0x10 -> -4)
      load_from_tdr(8'h10, 1'b1, 1'b1);          // EN=1, down
      repeat (10) @(posedge PCLK);
      apb_read_wait(`TCNT_ADDR, data_r, err);
      $display("[real_count] down/non-wrap TCNT=0x%0h", data_r);

      // (d) Count DOWN wrap (start=0x00 -> wrap về 0xFF, UDF pulse)
      load_from_tdr(8'h00, 1'b1, 1'b1);          // EN=1, down
      repeat (20) @(posedge PCLK);
      $display("[real_count] down/wrap - check flags (expect UDF=1)");
      check_flags(0,1);                          // mong OVF=0, UDF=1
      apb_write_wait(`TSR_ADDR, (1<<`TMR_UDF_bit)); // clear UDF
    end
  endtask

  // 4) Ghi địa chỉ invalid để cover default/PSLVERR
  task hit_invalid_addr_default;
    begin
      $display("\n[default_case] write invalid address 0xAA");
      apb_write_wait(8'hAA, 8'h5A); // không trùng TDR/TCR/TSR -> default
    end
  endtask

  // 5) Thử cover điều kiện PREADY==0 (nếu RTL có wait-state)
  task try_pready_zero;
    begin
      $display("\n[pready_zero] try nowait write");
      apb_write_nowait(`TDR_ADDR, 8'h33);
    end
  endtask

  // --- Exercise CKS values so select_clock is covered ---
  task cycle_through_cks;
    integer ci; reg [DATA_WIDTH-1:0] rd; reg e;
    begin
      $display("\n--- cycle_through_cks: exercise CKS 00..11 ---");
      for (ci=0; ci<4; ci=ci+1) begin
        // write TCR with CKS=ci, EN=0 (just set clk selection)
        apb_write_wait(`TCR_ADDR, make_tcr(ci[1:0], 1'b0, 1'b0, 1'b0));
        // short wait to let select_clock sample
        repeat (6) @(posedge PCLK);
        // read TCNT to cause bus activity and give coverage in read path
        apb_read_wait(`TCNT_ADDR, rd, e);
        $display("  CKS=%02b => TCNT_read=0x%0h", ci[1:0], rd);
      end
    end
  endtask

  // Tăng thời gian chờ để thật sự gây OVF/UDF
  task run_real_count_branches_long;
    integer i;
    begin
      $display("\n[real_count_long] exercise more cycles for wrap");

      // (a) Count UP wrap (start=0xFF -> wrap to 0)
      load_from_tdr(8'hFF, 1'b0, 1'b1);    // EN=1, up
      // increase wait to allow several internal clocks to tick and cause wrap
      repeat (80) @(posedge PCLK);
      $display("[real_count_long] up/wrap - check flags (expect OVF=1)");
      check_flags(1,0);
      apb_write_wait(`TSR_ADDR, (1 << `TMR_OVF_bit)); // clear

      // (b) Count DOWN wrap (start=0x00 -> wrap to 0xFF)
      load_from_tdr(8'h00, 1'b1, 1'b1);    // EN=1, down
      repeat (80) @(posedge PCLK);
      $display("[real_count_long] down/wrap - check flags (expect UDF=1)");
      check_flags(0,1);
      apb_write_wait(`TSR_ADDR, (1 << `TMR_UDF_bit)); // clear
    end
  endtask

  // ---------------- Main ----------------
  initial begin
    gen_reset();

    run_fake_underflow();        // EN=0 path, UDF không bật
    run_fake_overflow();         // EN=0 path, OVF không bật

    // exercise select_clock CKS choices
    cycle_through_cks();

    // dùng bản dài hơn để chắc wrap xuất hiện
    run_real_count_branches();   // ngắn
    run_real_count_branches_long(); // dài để chắc chắn wrap

    hit_invalid_addr_default();  // cover default/PSLVERR
    try_pready_zero();           // cố gắng hit nowait (nếu DUT hỗ trợ)

    #200 $finish;
  end

endmodule
