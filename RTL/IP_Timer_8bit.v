`default_nettype none

`include "Def_name.v"
`include "Rw_register.v"
`include "Control_logic.v"
`include "select_clock.v"
`include "Timer_counter.v"

module timer_top #(
    parameter ADDR_WIDTH = 8,
    parameter DATA_WIDTH = 8
) (
  input  wire                   PCLK,
  input  wire                   PRESET_n,
  input  wire                   PSEL,
  input  wire                   PWRITE,
  input  wire                   PENABLE,
  input  wire [ADDR_WIDTH-1:0]  PADDR,
  input  wire [DATA_WIDTH-1:0]  PWDATA,
  output wire [DATA_WIDTH-1:0]  PRDATA,
  output wire                   PREADY,
  output wire                   PSLVERR,
  output wire                   TMR_URF,
  output wire                   TMR_OVF
);
    // Wires
    wire [DATA_WIDTH-1:0] TDR_reg, TCR_reg, TSR_reg;
    wire                  TDR_WR_pulse;
    wire [DATA_WIDTH-1:0] TCNT_Out;
    wire [1:0]            Cks;
    wire                  Load_Tdr;
    wire [DATA_WIDTH-1:0] count_start_value;
    wire                  Clock_counter;
    wire                  count_enable, count_up_down;

    Rw_register #(.ADDR_WIDTH(ADDR_WIDTH), .DATA_WIDTH(DATA_WIDTH)) u_apb (
      .PCLK(PCLK), .PRESET_n(PRESET_n),
      .PSEL(PSEL), .PENABLE(PENABLE), .PWRITE(PWRITE),
      .PADDR(PADDR), .PWDATA(PWDATA), .PRDATA(PRDATA),
      .PREADY(PREADY), .PSLVERR(PSLVERR),
      .TDR_reg(TDR_reg), .TCR_reg(TCR_reg), .TSR_reg(TSR_reg),
      .TDR_WR_pulse(TDR_WR_pulse),
      .TCNT_Out(TCNT_Out),
      .Set_OVF_pulse(TMR_OVF), .Set_UDF_pulse(TMR_URF)
    );

    control_logic u_ctrl (
        .PCLK(PCLK),
        .PRESET_n(PRESET_n),
        .TDR_reg(TDR_reg),
        .TCR_reg(TCR_reg),
        .TDR_WR_pulse(TDR_WR_pulse),
        .Cks(Cks),
        .Load_Tdr(Load_Tdr),
        .count_start_value(count_start_value),
        .count_up_down(count_up_down),
        .count_enable(count_enable)
    );

    select_clock u_selclk (
        .PCLK(PCLK), .PRESET_n(PRESET_n),
        .Cks(Cks), .Clock_counter(Clock_counter)
    );

    timer_counter #(.DATA_WIDTH(DATA_WIDTH)) u_cnt (
        .PCLK(PCLK), .PRESET_n(PRESET_n),
        .Clock_counter(Clock_counter),
        .count_start_value(count_start_value),
        .count_load(Load_Tdr),
        .count_enable(count_enable),
        .count_up_down(count_up_down),
        .TCNT_Out(TCNT_Out),
        .Set_OVF_pulse(TMR_OVF), .Set_UDF_pulse(TMR_URF)
    );

endmodule
