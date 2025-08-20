// countup_forkjoin_pclk2.v - testbench (CKS loop + clock_counter driver)
`timescale 1ns/1ps
module countup_forkjoin_pclk2;
  `include "Def_name.v"

  // ensure DATA_WIDTH is defined
  `ifndef DATA_WIDTH
    `define DATA_WIDTH 8
  `endif

  localparam ADDR_WIDTH = 8;

  // APB signals
  reg PCLK;
  reg PRESET_n;
  reg PSEL;
  reg PWRITE;
  reg PENABLE;
  reg [ADDR_WIDTH-1:0] PADDR;
  reg [`DATA_WIDTH-1:0] PWDATA;
  wire [`DATA_WIDTH-1:0] PRDATA;
  wire PREADY;
  wire PSLVERR;
  wire TMR_OVF;
  wire TMR_URF;

  // test variables (declared at top)
  reg th1_pass;
  reg th2_pass;
  reg [`DATA_WIDTH-1:0] start_val;
  reg [`DATA_WIDTH-1:0] tsr_val;
  integer steps, factor, th1_wait, th2_wait;
  integer t;
  integer err_cnt;
  integer _cf;

  // debug/extra regs used inside initial/thread
  reg [`DATA_WIDTH-1:0] dbg_tcnt;
  reg [`DATA_WIDTH-1:0] dbg_tcr;
  reg [`DATA_WIDTH-1:0] tcr_rb;
  reg pslv_sample;
  reg [`DATA_WIDTH-1:0] readback_invalid;

  // loop/control vars
  integer cks_i;
  reg [1:0] cks_val;
  integer pulses_needed;
  reg loop_th1_pass;
  reg loop_th2_pass;

  // instantiate DUT (include DATA_WIDTH)
  timer_top #(.ADDR_WIDTH(ADDR_WIDTH), .DATA_WIDTH(`DATA_WIDTH)) dut (
    .PCLK(PCLK),
    .PRESET_n(PRESET_n),
    .PSEL(PSEL),
    .PWRITE(PWRITE),
    .PENABLE(PENABLE),
    .PADDR(PADDR),
    .PWDATA(PWDATA),
    .PRDATA(PRDATA),
    .PREADY(PREADY),
    .PSLVERR(PSLVERR),
    .TMR_OVF(TMR_OVF),
    .TMR_URF(TMR_URF)
  );

  // 100 MHz clock
  initial begin
    PCLK = 1'b0;
    forever #5 PCLK = ~PCLK;
  end

  // reset task
  task do_reset();
    begin
      PRESET_n = 1'b0;
      PSEL = 1'b0;
      PWRITE = 1'b0;
      PENABLE = 1'b0;
      PADDR = {ADDR_WIDTH{1'b0}};
      PWDATA = {`DATA_WIDTH{1'b0}};
      repeat (5) @(posedge PCLK);
      PRESET_n = 1'b1;
      repeat (2) @(posedge PCLK);
    end
  endtask

  // APB write (simple blocking style)
  task apb_write(input [ADDR_WIDTH-1:0] addr, input [`DATA_WIDTH-1:0] data_in);
    begin
      @(posedge PCLK);
      PADDR  = addr;
      PWDATA = data_in;
      PWRITE = 1'b1;
      PSEL   = 1'b1;
      PENABLE= 1'b0; // SETUP
      @(posedge PCLK);
      PENABLE = 1'b1; // ACCESS
      // wait for slave ready
      while (!PREADY) @(posedge PCLK);
      @(posedge PCLK); // end of transfer
      PSEL   = 1'b0;
      PWRITE = 1'b0;
      PENABLE= 1'b0;
    end
  endtask

  // APB read (output parameter)
  task apb_read(input [ADDR_WIDTH-1:0] addr, output [`DATA_WIDTH-1:0] data_out);
    begin
      @(posedge PCLK);
      PADDR  = addr;
      PWRITE = 1'b0;
      PSEL   = 1'b1;
      PENABLE= 1'b0; // SETUP
      @(posedge PCLK);
      PENABLE = 1'b1; // ACCESS
      while (!PREADY) @(posedge PCLK);
      data_out = PRDATA;
      @(posedge PCLK);
      PSEL = 1'b0;
      PENABLE = 1'b0;
    end
  endtask

  // APB write with forced wait-state (to cover internal PREADY==0 path)
  task apb_write_with_forced_wait;
    input [ADDR_WIDTH-1:0] addr;
    input [`DATA_WIDTH-1:0] data_in;
    input integer wait_cycles;
    integer j;
    begin
      @(posedge PCLK);
      PADDR  = addr; PWDATA = data_in; PWRITE = 1'b1; PSEL = 1'b1; PENABLE = 1'b0;
      @(posedge PCLK);
      PENABLE = 1'b1;
      // force internal PREADY low (hierarchical name depends on Rw_register instance name)
      force dut.u_apb.PREADY = 1'b0;
      for (j = 0; j < wait_cycles; j = j + 1) @(posedge PCLK);
      release dut.u_apb.PREADY;
      while (!PREADY) @(posedge PCLK);
      @(posedge PCLK);
      PSEL = 1'b0; PWRITE = 1'b0; PENABLE = 1'b0;
    end
  endtask

  // APB read with forced wait-state
  task apb_read_with_forced_wait;
    input [ADDR_WIDTH-1:0] addr;
    output [`DATA_WIDTH-1:0] data_out;
    input integer wait_cycles;
    integer j;
    begin
      @(posedge PCLK);
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
    end
  endtask

  // ----------------- NEW helpers: sample PSLVERR/PRDATA exactly when PREADY asserted -----------------
  task apb_write_check(
    input [ADDR_WIDTH-1:0] addr,
    input [`DATA_WIDTH-1:0] data_in,
    output reg pslv_out
  );
    begin
      @(posedge PCLK);
      PADDR  = addr; PWDATA = data_in; PWRITE = 1'b1; PSEL = 1'b1; PENABLE = 1'b0;
      @(posedge PCLK);
      PENABLE = 1'b1;
      while (!PREADY) @(posedge PCLK);
      pslv_out = PSLVERR;            // sample exactly when PREADY is true
      @(posedge PCLK);
      PSEL = 1'b0; PWRITE = 1'b0; PENABLE = 1'b0;
    end
  endtask

  task apb_read_check(
    input [ADDR_WIDTH-1:0] addr,
    output reg [`DATA_WIDTH-1:0] data_out,
    output reg pslv_out
  );
    begin
      @(posedge PCLK);
      PADDR  = addr; PWRITE = 1'b0; PSEL = 1'b1; PENABLE = 1'b0;
      @(posedge PCLK);
      PENABLE = 1'b1;
      while (!PREADY) @(posedge PCLK);
      pslv_out = PSLVERR;            // sample PSLVERR at PREADY
      data_out = PRDATA;
      @(posedge PCLK);
      PSEL = 1'b0; PENABLE = 1'b0;
    end
  endtask
  // ---------------------------------------------------------------------------------------------------

  // helper: build TCR value
  function [`DATA_WIDTH-1:0] make_tCR;
    input load;
    input up;
    input en;
    input [1:0] cks;
    reg [`DATA_WIDTH-1:0] v;
    begin
      v = {`DATA_WIDTH{1'b0}};
      v[`TCR_load_bit]     = load;
      v[`TCR_Up_Dw_bit]    = up;
      v[`TCR_En_bit]       = en;
      v[`TCR_Cks_1:`TCR_Cks_0] = cks;
      make_tCR = v;
    end
  endfunction

  // clear TSR (write clear bits)
  task clear_tsr;
    reg [`DATA_WIDTH-1:0] d;
    begin
      apb_write(`TSR_ADDR, 8'b0000_0001); // clear OVF
      apb_write(`TSR_ADDR, 8'b0000_0010); // clear UDF
      apb_read(`TSR_ADDR, d);
    end
  endtask

  // program and start: write TDR, cause load, enable counting
  task program_and_start(input [`DATA_WIDTH-1:0] start_val, input up, input [1:0] cks);
    begin
      clear_tsr();
      apb_write(`TDR_ADDR, start_val);
      // generate explicit load rising edge
      apb_write(`TCR_ADDR, make_tCR(1'b1, up, 1'b0, cks)); // load = 1
      @(posedge PCLK);
      apb_write(`TCR_ADDR, make_tCR(1'b0, up, 1'b1, cks)); // clear load and enable counting
      @(posedge PCLK);
    end
  endtask

  // helper functions for counts
  function integer steps_to_event_up(input [`DATA_WIDTH-1:0] start);
    begin
      steps_to_event_up = ( (1 << `DATA_WIDTH) - start );
    end
  endfunction

  // cks_factor
  function integer cks_factor(input [1:0] cks);
    begin
      case (cks)
        2'b00: cks_factor = 2;
        2'b01: cks_factor = 4;
        2'b10: cks_factor = 8;
        default: cks_factor = 16;
      endcase
    end
  endfunction

  // ----------------- NEW: drive Clock_counter pulses via hierarchical force -----------------
  task drive_clock_counter_pulses(input integer n_pulses);
    integer p;
    begin
      // baseline low
      force dut.u_selclk.Clock_counter = 1'b0;
      @(posedge PCLK);
      for (p = 0; p < n_pulses; p = p + 1) begin
        // create a 0->1 transition sampled by the timer logic
        force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);
        force dut.u_selclk.Clock_counter = 1'b1; @(posedge PCLK);
        force dut.u_selclk.Clock_counter = 1'b0; @(posedge PCLK);
      end
      release dut.u_selclk.Clock_counter;
      repeat (2) @(posedge PCLK); // let logic settle
    end
  endtask
  // --------------------------------------------------------------------------------------------

  initial begin
    err_cnt = 0;
    do_reset();

    // We'll iterate CKS = 00,01,10,11 to cover select_clock branches.
    for (cks_i = 0; cks_i < 4; cks_i = cks_i + 1) begin
      cks_val = cks_i[1:0];
      // pick a random start < 255
      start_val = $urandom_range(0,254);
      $display("\n=== TEST for CKS = %b, start=%0d ===", cks_val, start_val);

      // program and start timer: up counting, selected CKS
      program_and_start(start_val, 1'b0, cks_val);
      apb_write_with_forced_wait(`TDR_ADDR, $urandom, 3);   // ép PREADY=0 trên WRITE
      apb_read_with_forced_wait (`TCR_ADDR, tsr_val, 3);
      // compute how many PCLK cycles we should generate (approx)
      steps  = steps_to_event_up(start_val);    // number of counter increments needed
      factor = cks_factor(cks_val);             // ticks per increment (approx)
      pulses_needed = steps * factor + 40;      // margin

      // compute thread timeouts (in PCLK cycles). Use pulses_needed as approximate upper bound.
      th1_wait = pulses_needed + 100;
      th2_wait = (th1_wait * 2) / 3;

      // reset per-loop flags
      loop_th1_pass = 0;
      loop_th2_pass = 0;

      // run driver + 2 monitoring threads in parallel
      fork
        // driver: produce pulses_needed rising edges
        begin : driver
          drive_clock_counter_pulses(pulses_needed);
        end

        // thread1: wait for OVF
        begin : mon1
          for (t = 0; t < th1_wait; t = t + 1) begin
            @(posedge PCLK);
            if (TMR_OVF) begin
              loop_th1_pass = 1;
              $display("CKS=%b Thread1: observed TMR_OVF at time %0t", cks_val, $time);
              disable mon1;
            end
          end
          if (!loop_th1_pass) begin
            apb_read(`TCNT_ADDR, dbg_tcnt);
            apb_read(`TCR_ADDR, dbg_tcr);
            $display("CKS=%b Thread1 TIMEOUT: TCNT=0x%0h, TCR=0x%0h, steps=%0d factor=%0d pulses_needed=%0d",
                     cks_val, dbg_tcnt, dbg_tcr, steps, factor, pulses_needed);
            $display("Hint: check TCR.EN and CKS bits; if EN=0 timer won't count.");
            err_cnt = err_cnt + 1;
          end
        end

        // thread2: earlier sample of TSR (should NOT have OVF)
        begin : mon2
          repeat (th2_wait) @(posedge PCLK);
          apb_read(`TSR_ADDR, tsr_val);
          if (tsr_val[`TMR_OVF_bit]) begin
            $display("CKS=%b Thread2: FAULT - TSR shows OVF too early TSR=0x%0h", cks_val, tsr_val);
            err_cnt = err_cnt + 1;
          end else begin
            loop_th2_pass = 1;
            $display("CKS=%b Thread2: PASS - no OVF at earlier time TSR=0x%0h", cks_val, tsr_val);
          end
        end
      join

      // report per-cks status
      if (loop_th1_pass && loop_th2_pass) begin
        $display("CKS=%b: PASS (thread1 & thread2 ok)", cks_val);
      end else begin
        $display("CKS=%b: FAIL (th1=%0d th2=%0d) err_cnt=%0d", cks_val, loop_th1_pass, loop_th2_pass, err_cnt);
      end

      // small gap between CKS runs
      repeat (4) @(posedge PCLK);
    end // for cks loop

    // final coverage helpers / debug

    // call cks_factor to hit all branches (coverage)
    _cf = cks_factor(2'b00); $display("DBG: cks_factor(00) = %0d", _cf);
    _cf = cks_factor(2'b01); $display("DBG: cks_factor(01) = %0d", _cf);
    _cf = cks_factor(2'b10); $display("DBG: cks_factor(10) = %0d", _cf);
    _cf = cks_factor(2'b11); $display("DBG: cks_factor(11) = %0d", _cf);

    // verify TCR readback (ensure EN and CKS saved)
    apb_read(`TCR_ADDR, tcr_rb);
    $display("DBG: TCR readback = 0x%0h (EN=%b CKS=%b)", tcr_rb, tcr_rb[`TCR_En_bit], tcr_rb[`TCR_Cks_1:`TCR_Cks_0]);
    if (tcr_rb[`TCR_En_bit] !== 1'b1) begin
      $display("WARN: TCR.EN not set/readback -> timer might not be enabled (affects coverage)");
    end

    // Also exercise select_clock via TCR writes (optional)
    apb_write(`TCR_ADDR, make_tCR(1'b0, 1'b1, 1'b1, 2'b00)); @(posedge PCLK);
    apb_write(`TCR_ADDR, make_tCR(1'b0, 1'b1, 1'b1, 2'b01)); @(posedge PCLK);
    apb_write(`TCR_ADDR, make_tCR(1'b0, 1'b1, 1'b1, 2'b10)); @(posedge PCLK);
    apb_write(`TCR_ADDR, make_tCR(1'b0, 1'b1, 1'b1, 2'b11)); @(posedge PCLK);
    repeat (3) @(posedge PCLK);

    // ---------- Exercise Rw_register default (invalid address) ----------
    apb_write_check(8'h55, 8'hDE, pslv_sample);
    if (pslv_sample) begin
      $display("DBG: invalid write to 0x55 -> PSLVERR asserted (hit default write-case)");
    end else begin
      $display("ERROR: invalid write to 0x55 did NOT assert PSLVERR -> Rw_register.default NOT covered");
      err_cnt = err_cnt + 1;
    end

    apb_read_check(8'h55, readback_invalid, pslv_sample);
    $display("DBG: invalid read from 0x55 => PSLVERR=%b PRDATA=%0h", pslv_sample, readback_invalid);
    if (!pslv_sample) begin
      $display("WARN: PSLVERR not asserted on invalid read; check Rw_register.default read-case and PSLVERR behavior");
    end

    if (err_cnt == 0) $display("\ncountup_forkjoin_pclk2: ALL TESTS PASSED (err_cnt=0)");
    else $display("\ncountup_forkjoin_pclk2: FINISHED with err_cnt=%0d", err_cnt);

    #200 $finish;
  end

endmodule
