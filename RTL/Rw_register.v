`timescale 1ns/1ps
`default_nettype none
`include "Def_name.v"

module Rw_register #(
  parameter ADDR_WIDTH = 8,
  parameter DATA_WIDTH = 8
)(
  input  wire                   PCLK,
  input  wire                   PRESET_n,
  input  wire                   PSEL,
  input  wire                   PWRITE,
  input  wire                   PENABLE,
  input  wire [ADDR_WIDTH-1:0]  PADDR,
  input  wire [DATA_WIDTH-1:0]  PWDATA,
  output reg  [DATA_WIDTH-1:0]  PRDATA,
  output reg                    PREADY,
  output reg                    PSLVERR,

  // registers exposed
  output reg [DATA_WIDTH-1:0] TDR_reg,
  output reg [DATA_WIDTH-1:0] TCR_reg,
  output reg [DATA_WIDTH-1:0] TSR_reg,
  output reg                   TDR_WR_pulse,

  // counter input (from timer_counter)
  input  wire [DATA_WIDTH-1:0] TCNT_Out,
  input  wire                  Set_OVF_pulse,
  input  wire                  Set_UDF_pulse
);

  // internal combinational PRDATA bus
  reg [DATA_WIDTH-1:0] PRDATA_next;

  // internal temp for TSR update
  reg [DATA_WIDTH-1:0] tsr_next;

  // For simulation: optional one-cycle random wait-state generator
  `ifdef SIM
    reg pready_gen;
  `endif

  // PREADY/PSLVERR: default behavior, but support sim wait-states under `SIM`
  always @(posedge PCLK or negedge PRESET_n) begin
    if (!PRESET_n) begin
      PREADY  <= 1'b1;
      PSLVERR <= 1'b0;
      `ifdef SIM
        pready_gen <= 1'b1;
      `endif
    end else begin
      `ifdef SIM
        // simple heuristic: when there is a bus transaction we *occasionally*
        // insert one cycle of PREADY==0 to exercise wait-state branches.
        if (PSEL && PENABLE) begin
          // $random returns 32-bit; pick low entropy bit to sometimes create 0
          if (($random & 3) == 0) pready_gen <= 1'b0;
          else pready_gen <= 1'b1;
        end else begin
          pready_gen <= 1'b1;
        end
        PREADY <= pready_gen;
      `else
        PREADY <= 1'b1;
      `endif
      PSLVERR <= 1'b0;
    end
  end

  // APB write handling (synchronous)
  // generate 1-cycle pulses for TDR_WR_pulse when TDR written
  reg tdr_wr_pulse_r;
  always @(posedge PCLK or negedge PRESET_n) begin
    if (!PRESET_n) begin
      TDR_reg        <= {DATA_WIDTH{1'b0}};
      TCR_reg        <= {DATA_WIDTH{1'b0}};
      TSR_reg        <= {DATA_WIDTH{1'b0}};
      tdr_wr_pulse_r <= 1'b0;
      TDR_WR_pulse   <= 1'b0;
      tsr_next       <= {DATA_WIDTH{1'b0}};
      PSLVERR        <= 1'b0;
    end else begin
      // default clear pulse (internal)
      tdr_wr_pulse_r <= 1'b0;
      TDR_WR_pulse   <= 1'b0;
      // start from current TSR value (working copy)
      tsr_next = TSR_reg;

      if (PSEL && PENABLE && PWRITE && PREADY) begin
        `ifdef SIM
          $display("%0t Rw_register: APB_WRITE addr=0x%0h data=0x%0h", $time, PADDR, PWDATA);
        `endif
        case (PADDR)
          `TDR_ADDR: begin
            TDR_reg <= PWDATA;
            tdr_wr_pulse_r <= 1'b1; // assert internal pulse
          end

          `TCR_ADDR: begin
            // mask bits: [7]=Load, [6]=0, [5]=Up/Dw, [4]=En, [3:2]=0, [1:0]=Cks
            TCR_reg <= { PWDATA[7], 1'b0, PWDATA[5], PWDATA[4], 2'b00, PWDATA[1:0] };
          end

          `TSR_ADDR: begin
            // write-one-to-clear: clear bits that are '1' in PWDATA
            tsr_next = TSR_reg & ~PWDATA;
          end

          default: begin
            // invalid address write -> set PSLVERR for 1 cycle
            PSLVERR <= 1'b1;
          end
        endcase
      end

      // Merge hardware pulses into TSR register (set bits when pulses occur)
      if (Set_OVF_pulse) tsr_next[`TMR_OVF_bit] = 1'b1;
      if (Set_UDF_pulse) tsr_next[`TMR_UDF_bit] = 1'b1;

      // update TSR_reg once at clock edge
      TSR_reg <= tsr_next;

      // propagate the internal one-clock pulse to output after registers updated
      if (tdr_wr_pulse_r) begin
        TDR_WR_pulse <= 1'b1;
      end
    end
  end

  // Read path (combinational PRDATA mux)
  always @(*) begin
    case (PADDR)
      `TDR_ADDR:  PRDATA_next = TDR_reg;
      `TCR_ADDR:  PRDATA_next = TCR_reg;
      `TSR_ADDR:  PRDATA_next = TSR_reg;
      `TCNT_ADDR: PRDATA_next = TCNT_Out;
      default:    PRDATA_next = {DATA_WIDTH{1'b0}};
    endcase
  end

  // Drive PRDATA when read access occurs (sampled on PCLK)
  always @(posedge PCLK or negedge PRESET_n) begin
    if (!PRESET_n) begin
      PRDATA <= {DATA_WIDTH{1'b0}};
    end else begin
      if (PSEL && PENABLE && !PWRITE && PREADY) begin
        PRDATA <= PRDATA_next;
        `ifdef SIM
          $display("%0t Rw_register: APB_READ addr=0x%0h data_out=0x%0h", $time, PADDR, PRDATA_next);
        `endif
      end
    end
  end

endmodule

`default_nettype wire
